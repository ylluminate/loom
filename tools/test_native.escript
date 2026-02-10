#!/usr/bin/env escript
%% -*- erlang -*-
%%! -mode(compile)
%% V-to-Native Unified Test Harness
%%
%% Tests V programs through native compilation pipelines:
%%   --arch=x86_64  : V → IR → x86_64 ELF (Docker/QEMU/direct)
%%   --arch=arm64   : V → IR → ARM64 Mach-O/ELF (native macOS/QEMU)
%%   --arch=uefi    : V → IR → x86_64 PE (QEMU + OVMF)
%%
%% Usage:
%%   tools/test_native.escript --arch=x86_64              # All tests, x86_64 ELF
%%   tools/test_native.escript --arch=arm64 tests/native  # Tests in dir, ARM64
%%   tools/test_native.escript --arch=uefi hello.v        # Specific test, UEFI
%%   tools/test_native.escript --arch=x86_64 --verbose    # Verbose output
%%
%% Options:
%%   --arch=ARCH           Required: x86_64 | arm64 | uefi
%%   --verbose, -v         Show detailed output
%%   --keep, -k            Keep intermediate artifacts
%%   --timeout=SECONDS     Per-test timeout (default: 10 for x86_64/arm64, 15 for uefi)
%%   --docker=IMAGE        Docker image for x86_64 (default: thevlang/vlang:alpine)
%%   --ovmf=PATH           OVMF firmware path for UEFI tests
%%   --help, -h            Show this help

-mode(compile).

-define(BLUE,    "\033[0;34m").
-define(GREEN,   "\033[0;32m").
-define(RED,     "\033[0;31m").
-define(YELLOW,  "\033[0;33m").
-define(CYAN,    "\033[0;36m").
-define(NC,      "\033[0m").

-record(opts, {
    arch                :: x86_64 | arm64 | uefi,
    verbose = false     :: boolean(),
    keep = false        :: boolean(),
    timeout             :: integer(),
    docker_image        :: string(),
    ovmf_path           :: string() | undefined,
    test_paths = []     :: [string()]
}).

-record(state, {
    v_compiler          :: string(),
    project_root        :: string(),
    work_dir            :: string(),
    default_test_dir    :: string(),
    run_method          :: atom(),
    platform            :: atom(),
    total = 0           :: integer(),
    passed = 0          :: integer(),
    failed = 0          :: integer(),
    compile_errors = 0  :: integer(),
    ir_errors = 0       :: integer(),
    link_errors = 0     :: integer(),
    runtime_errors = 0  :: integer(),
    failed_tests = []   :: [string()]
}).

%% Main entry point
main(Args) ->
    try
        Opts = parse_args(Args, #opts{}),
        validate_opts(Opts),
        run_tests(Opts)
    catch
        throw:{error, Msg} ->
            log_error(Msg),
            halt(2);
        error:Reason:Stack ->
            log_error(io_lib:format("Internal error: ~p~n~p", [Reason, Stack])),
            halt(2)
    end.

%% Parse command-line arguments
parse_args([], Opts) -> Opts;
parse_args(["--arch=" ++ Arch | Rest], Opts) ->
    ArchAtom = list_to_atom(Arch),
    case lists:member(ArchAtom, [x86_64, arm64, uefi]) of
        true -> parse_args(Rest, Opts#opts{arch = ArchAtom});
        false -> throw({error, "Invalid architecture: " ++ Arch ++ ". Use x86_64, arm64, or uefi"})
    end;
parse_args(["-v" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{verbose = true});
parse_args(["--verbose" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{verbose = true});
parse_args(["-k" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{keep = true});
parse_args(["--keep" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{keep = true});
parse_args(["--timeout=" ++ Timeout | Rest], Opts) ->
    parse_args(Rest, Opts#opts{timeout = list_to_integer(Timeout)});
parse_args(["--docker=" ++ Image | Rest], Opts) ->
    parse_args(Rest, Opts#opts{docker_image = Image});
parse_args(["--ovmf=" ++ Path | Rest], Opts) ->
    parse_args(Rest, Opts#opts{ovmf_path = Path});
parse_args(["-h" | _], _Opts) ->
    show_help(),
    halt(0);
parse_args(["--help" | _], _Opts) ->
    show_help(),
    halt(0);
parse_args([Arg | Rest], Opts) ->
    case string:prefix(Arg, "--") of
        nomatch ->
            parse_args(Rest, Opts#opts{test_paths = Opts#opts.test_paths ++ [Arg]});
        _ ->
            throw({error, "Unknown option: " ++ Arg})
    end.

%% Validate options
validate_opts(#opts{arch = undefined}) ->
    throw({error, "Missing required option: --arch=ARCH (x86_64|arm64|uefi)"});
validate_opts(_) ->
    ok.

%% Show help
show_help() ->
    io:format("V-to-Native Unified Test Harness~n~n"),
    io:format("Usage:~n"),
    io:format("  test_native.escript --arch=ARCH [OPTIONS] [test_path...]~n~n"),
    io:format("Options:~n"),
    io:format("  --arch=ARCH           Required: x86_64 | arm64 | uefi~n"),
    io:format("  --verbose, -v         Show detailed output~n"),
    io:format("  --keep, -k            Keep intermediate artifacts~n"),
    io:format("  --timeout=SECONDS     Per-test timeout (default: 10 for x86_64/arm64, 15 for uefi)~n"),
    io:format("  --docker=IMAGE        Docker image for x86_64 (default: thevlang/vlang:alpine)~n"),
    io:format("  --ovmf=PATH           OVMF firmware path for UEFI tests~n"),
    io:format("  --help, -h            Show this help~n~n"),
    io:format("Architectures:~n"),
    io:format("  x86_64   - x86_64 ELF (Linux via Docker/QEMU/native)~n"),
    io:format("  arm64    - ARM64 Mach-O (macOS) or ELF (Linux via QEMU)~n"),
    io:format("  uefi     - x86_64 PE UEFI (QEMU full-system + OVMF)~n~n"),
    io:format("Examples:~n"),
    io:format("  test_native.escript --arch=x86_64~n"),
    io:format("  test_native.escript --arch=arm64 tests/native/~n"),
    io:format("  test_native.escript --arch=uefi hello.v --verbose~n").

%% Main test runner
run_tests(Opts) ->
    %% Initialize paths
    ScriptDir = filename:dirname(escript:script_name()),
    ProjectRoot = filename:dirname(ScriptDir),
    VCompiler = "/Users/u/tank/ops/tools/dev/vlang/v",
    DefaultTestDir = filename:join(ProjectRoot, "tests/native"),

    %% Set defaults
    Timeout = case Opts#opts.timeout of
        undefined -> case Opts#opts.arch of
            uefi -> 15;
            _ -> 10
        end;
        T -> T
    end,

    DockerImage = case Opts#opts.docker_image of
        undefined -> "thevlang/vlang:alpine";
        I -> I
    end,

    TestPaths = case Opts#opts.test_paths of
        [] -> [DefaultTestDir];
        Paths -> Paths
    end,

    %% Validate V compiler
    case filelib:is_regular(VCompiler) of
        false -> throw({error, "V compiler not found at " ++ VCompiler});
        true -> ok
    end,

    %% Validate Erlang
    case os:find_executable("erl") of
        false -> throw({error, "Erlang/OTP not found. Install Erlang to use the native compiler."});
        _ -> ok
    end,

    %% Architecture-specific initialization
    State0 = #state{
        v_compiler = VCompiler,
        project_root = ProjectRoot,
        default_test_dir = DefaultTestDir
    },

    {RunMethod, Platform, State1, OptsWithOvmf} = case Opts#opts.arch of
        x86_64 ->
            {Method, Plat} = detect_x86_64_run_method(DockerImage),
            WorkDir = filename:join(ProjectRoot, "beam_output/native_x86_64_tests"),
            {Method, Plat, State0#state{work_dir = WorkDir, run_method = Method, platform = Plat}, Opts};
        arm64 ->
            {Method, Plat} = detect_arm64_platform(),
            WorkDir = filename:join(ProjectRoot, "beam_output/native_arm64_tests"),
            {Method, Plat, State0#state{work_dir = WorkDir, run_method = Method, platform = Plat}, Opts};
        uefi ->
            OvmfPath = validate_uefi_prerequisites(Opts#opts.ovmf_path),
            WorkDir = filename:join(ProjectRoot, "beam_output/native_uefi_tests"),
            {qemu_uefi, uefi, State0#state{work_dir = WorkDir, run_method = qemu_uefi, platform = uefi},
             Opts#opts{ovmf_path = OvmfPath}}
    end,

    %% Create work directory
    filelib:ensure_dir(filename:join(State1#state.work_dir, "dummy")),

    %% Print header
    print_header(Opts#opts.arch, VCompiler, RunMethod, Platform, Timeout),

    %% Collect test files
    TestFiles = collect_test_files(TestPaths),
    case TestFiles of
        [] -> throw({error, "No .v test files found"});
        _ -> ok
    end,

    io:format("Found ~p test file(s)~n~n", [length(TestFiles)]),

    %% Run tests
    State2 = lists:foldl(
        fun(TestFile, StateAcc) ->
            run_single_test(TestFile, OptsWithOvmf, StateAcc)
        end,
        State1,
        TestFiles
    ),

    %% Print summary
    print_summary(State2),

    %% Cleanup
    case Opts#opts.keep of
        false -> file:del_dir(State2#state.work_dir);
        true -> ok
    end,

    %% Exit with appropriate code
    TotalFailed = State2#state.failed + State2#state.compile_errors +
                  State2#state.ir_errors + State2#state.link_errors +
                  State2#state.runtime_errors,
    case TotalFailed of
        0 -> halt(0);
        _ -> halt(1)
    end.

%% Detect x86_64 execution method
detect_x86_64_run_method(DockerImage) ->
    {OsName, Arch} = get_os_arch(),

    %% Check for native x86_64 Linux
    case {OsName, Arch} of
        {linux, "x86_64"} ->
            log_info("Execution: direct (native x86_64 Linux)"),
            {direct, x86_64_linux};
        {linux, _} ->
            %% Check for QEMU user-mode
            case os:find_executable("qemu-x86_64") of
                false -> check_docker(DockerImage);
                _ ->
                    log_info("Execution: qemu-x86_64 user-mode"),
                    {qemu, x86_64_linux}
            end;
        _ ->
            check_docker(DockerImage)
    end.

check_docker(DockerImage) ->
    case os:find_executable("docker") of
        false -> throw({error, "No execution method available for x86_64 ELF binaries. Install Docker or QEMU."});
        _ ->
            %% Check if Docker is running
            case os:cmd("docker info 2>&1") of
                "Cannot connect" ++ _ ->
                    throw({error, "Docker daemon not running. Start OrbStack or Docker Desktop."});
                _ ->
                    %% Verify image exists
                    Cmd = "docker image inspect " ++ DockerImage ++ " >/dev/null 2>&1",
                    case os:cmd(Cmd) of
                        "" ->
                            log_info(io_lib:format("Execution: Docker --platform linux/amd64 (~s)", [DockerImage])),
                            {docker, x86_64_docker};
                        _ ->
                            Msg = "Docker image '" ++ DockerImage ++ "' not found. Pull it with: docker pull --platform linux/amd64 " ++ DockerImage,
                            throw({error, Msg})
                    end
            end
    end.

%% Detect ARM64 platform
detect_arm64_platform() ->
    {OsName, Arch} = get_os_arch(),

    case {OsName, Arch} of
        {darwin, "arm64"} ->
            log_info("Platform: ARM64 macOS (native execution)"),
            {direct, arm64_macos};
        {darwin, "x86_64"} ->
            %% Check for Rosetta 2
            case os:cmd("sysctl -n sysctl.proc_translated 2>/dev/null") of
                "1\n" ->
                    log_info("Platform: x86_64 macOS under Rosetta 2 (native execution via translation)"),
                    {direct, arm64_macos_rosetta};
                _ ->
                    throw({error, "x86_64 macOS without Rosetta 2 cannot run ARM64 binaries. Install Rosetta 2: softwareupdate --install-rosetta"})
            end;
        {linux, "aarch64"} ->
            log_info("Platform: ARM64 Linux (native execution)"),
            {direct, arm64_linux};
        {linux, "x86_64"} ->
            case os:find_executable("qemu-aarch64") of
                false ->
                    throw({error, "x86_64 Linux requires qemu-aarch64 for ARM64 emulation. Install: sudo apt install qemu-user qemu-user-static"});
                _ ->
                    log_info("Platform: x86_64 Linux (ARM64 via qemu-aarch64 user-mode emulation)"),
                    {qemu_aarch64, x86_64_linux_qemu}
            end;
        _ ->
            throw({error, io_lib:format("Unsupported platform: ~s ~s", [OsName, Arch])})
    end.

%% Validate UEFI prerequisites
validate_uefi_prerequisites(OvmfPathOpt) ->
    %% Check qemu-system-x86_64
    case os:find_executable("qemu-system-x86_64") of
        false ->
            throw({error, "qemu-system-x86_64 not found. Install: brew install qemu (macOS) or sudo apt install qemu-system-x86 (Ubuntu)"});
        _ -> ok
    end,

    %% Find OVMF firmware
    SearchPaths = [
        "/usr/share/OVMF/OVMF_CODE.fd",
        "/usr/share/edk2/ovmf/OVMF_CODE.fd",
        "/usr/share/qemu/OVMF.fd",
        "/usr/share/ovmf/OVMF.fd",
        "/opt/homebrew/share/qemu/edk2-x86_64-code.fd",
        "/usr/local/share/qemu/edk2-x86_64-code.fd"
    ],

    OvmfPath = case OvmfPathOpt of
        undefined ->
            case lists:filter(fun filelib:is_regular/1, SearchPaths) of
                [] ->
                    throw({error, "OVMF firmware not found. Install: brew install qemu (macOS) or sudo apt install ovmf (Ubuntu)"});
                [P | _] -> P
            end;
        P ->
            case filelib:is_regular(P) of
                false -> throw({error, "OVMF firmware not found at specified path: " ++ P});
                true -> P
            end
    end,

    log_info("OVMF firmware: " ++ OvmfPath),
    OvmfPath.

%% Get OS name and architecture
get_os_arch() ->
    OsName = case os:type() of
        {unix, darwin} -> darwin;
        {unix, linux} -> linux;
        _ -> unknown
    end,
    Arch = string:trim(os:cmd("uname -m")),
    {OsName, Arch}.

%% Collect test files from paths
collect_test_files(Paths) ->
    lists:flatmap(
        fun(Path) ->
            case filelib:is_regular(Path) of
                true ->
                    case filename:extension(Path) of
                        ".v" -> [Path];
                        _ -> []
                    end;
                false ->
                    case filelib:is_dir(Path) of
                        true ->
                            Files = filelib:wildcard(filename:join(Path, "*.v")),
                            lists:sort(Files);
                        false ->
                            log_warn("Path not found: " ++ Path),
                            []
                    end
            end
        end,
        Paths
    ).

%% Run a single test
run_single_test(VFile, Opts, State) ->
    TestName = filename:basename(VFile, ".v"),
    TestDir = filename:dirname(VFile),

    %% Create work directory for this test
    WorkPath = filename:join(State#state.work_dir, TestName),
    file:del_dir_r(WorkPath),
    filelib:ensure_dir(filename:join(WorkPath, "dummy")),

    log_verbose(Opts, "Testing: " ++ VFile),

    %% Compile V to native binary
    case compile_native(VFile, WorkPath, Opts, State) of
        {ok, BinaryPath} ->
            %% Run the binary
            case run_native_binary(BinaryPath, Opts, State) of
                {ok, ActualOutput} ->
                    %% Compare with expected output
                    ExpectedFile = filename:join(TestDir, TestName ++ ".expected"),
                    case filelib:is_regular(ExpectedFile) of
                        true ->
                            {ok, ExpectedBin} = file:read_file(ExpectedFile),
                            Expected = binary_to_list(ExpectedBin),
                            case compare_output(ActualOutput, Expected) of
                                true ->
                                    log_pass(TestName),
                                    State#state{total = State#state.total + 1, passed = State#state.passed + 1};
                                false ->
                                    log_fail(TestName ++ " (Output mismatch)"),
                                    log_verbose(Opts, "  Expected: " ++ Expected),
                                    log_verbose(Opts, "  Actual: " ++ ActualOutput),
                                    State#state{
                                        total = State#state.total + 1,
                                        failed = State#state.failed + 1,
                                        failed_tests = State#state.failed_tests ++ [TestName ++ ": output mismatch"]
                                    }
                            end;
                        false ->
                            log_pass(TestName ++ " (no .expected file, ran without error)"),
                            log_verbose(Opts, "  Output: " ++ ActualOutput),
                            State#state{total = State#state.total + 1, passed = State#state.passed + 1}
                    end;
                {error, timeout} ->
                    log_fail(TestName ++ " (Timeout)"),
                    State#state{
                        total = State#state.total + 1,
                        runtime_errors = State#state.runtime_errors + 1,
                        failed_tests = State#state.failed_tests ++ [TestName ++ ": timeout"]
                    };
                {error, {runtime, ExitCode, Output}} ->
                    log_fail(io_lib:format("~s (Runtime error: exit code ~p)", [TestName, ExitCode])),
                    log_verbose(Opts, "  Output: " ++ Output),
                    State#state{
                        total = State#state.total + 1,
                        runtime_errors = State#state.runtime_errors + 1,
                        failed_tests = State#state.failed_tests ++ [io_lib:format("~s: runtime error (exit ~p)", [TestName, ExitCode])]
                    }
            end;
        {error, v_compile, Output} ->
            log_fail(TestName ++ " (V compilation failed)"),
            log_verbose(Opts, "  Error: " ++ Output),
            State#state{
                total = State#state.total + 1,
                compile_errors = State#state.compile_errors + 1,
                failed_tests = State#state.failed_tests ++ [TestName ++ ": V compilation error"]
            };
        {error, no_ir, Output} ->
            log_fail(TestName ++ " (No native IR generated)"),
            log_verbose(Opts, "  Error: " ++ Output),
            State#state{
                total = State#state.total + 1,
                ir_errors = State#state.ir_errors + 1,
                failed_tests = State#state.failed_tests ++ [TestName ++ ": no native IR"]
            };
        {error, link, Output} ->
            log_fail(TestName ++ " (Native link/assemble failed)"),
            log_verbose(Opts, "  Error: " ++ Output),
            State#state{
                total = State#state.total + 1,
                link_errors = State#state.link_errors + 1,
                failed_tests = State#state.failed_tests ++ [TestName ++ ": native link error"]
            }
    end.

%% Compile V source to native binary
compile_native(VFile, WorkPath, Opts, State) ->
    TestName = filename:basename(VFile, ".v"),
    VDir = filename:dirname(VFile),
    BeamOutDir = filename:join(VDir, TestName ++ ".beam"),

    %% Clean previous output
    file:del_dir_r(BeamOutDir),

    %% Determine target and format
    {Target, Format} = case Opts#opts.arch of
        x86_64 -> {"x86_64", "elf64"};
        arm64 ->
            case State#state.platform of
                arm64_macos -> {"arm64", "macho"};
                arm64_macos_rosetta -> {"arm64", "macho"};
                x86_64_linux_qemu -> {"arm64", "elf64"};
                arm64_linux -> {"arm64", "elf64"}
            end;
        uefi -> {"x86_64", "pe"}
    end,

    %% Step 1: Compile V to native IR
    log_verbose(Opts, io_lib:format("  V -> Native IR (target=~s, format=~s)...", [Target, Format])),
    VCmd = io_lib:format("VBEAM_TARGET=~s VBEAM_FORMAT=~s ~s -b beam ~s 2>&1",
                         [Target, Format, State#state.v_compiler, VFile]),
    VOutput = os:cmd(VCmd),

    %% Check for V compilation error
    case string:find(VOutput, "error:") of
        nomatch ->
            %% Check that IR file was generated
            IrFile = filename:join(BeamOutDir, ".vbeam_native.ir"),
            case filelib:is_regular(IrFile) of
                true ->
                    %% Step 2: Compile IR to native binary
                    log_verbose(Opts, "  Native IR -> binary..."),
                    OutFile = case Opts#opts.arch of
                        uefi -> filename:join(WorkPath, "BOOTX64.EFI");
                        _ -> filename:join(WorkPath, "a.out")
                    end,

                    %% Find ebin directory
                    VBeamRtDir = filename:join(State#state.project_root, "vbeam_rt"),
                    EbinDir = case filelib:is_dir(filename:join([VBeamRtDir, "_build/default/lib/vbeam_rt/ebin"])) of
                        true -> filename:join([VBeamRtDir, "_build/default/lib/vbeam_rt/ebin"]);
                        false -> filename:join(VBeamRtDir, "ebin")
                    end,

                    FormatArg = case Opts#opts.arch of
                        uefi -> ["-format", "pe"];
                        _ -> []
                    end,

                    CompileCmd = io_lib:format("erl -noshell -pa ~s -eval \"vbeam_native:main([\\\"~s\\\", \\\"-o\\\", \\\"~s\\\"~s]), init:stop().\" 2>&1",
                                               [EbinDir, IrFile, OutFile, format_args(FormatArg)]),
                    CompileOutput = os:cmd(CompileCmd),

                    %% Clean up V compiler output
                    file:del_dir_r(BeamOutDir),

                    case filelib:is_regular(OutFile) of
                        true ->
                            Size = filelib:file_size(OutFile),
                            log_verbose(Opts, io_lib:format("  Binary produced: ~p bytes", [Size])),
                            {ok, OutFile};
                        false ->
                            {error, link, CompileOutput}
                    end;
                false ->
                    file:del_dir_r(BeamOutDir),
                    {error, no_ir, VOutput}
            end;
        _ ->
            file:del_dir_r(BeamOutDir),
            {error, v_compile, VOutput}
    end.

%% Format additional arguments for Erlang eval
format_args([]) -> "";
format_args(Args) ->
    ", " ++ string:join([io_lib:format("\\\"~s\\\"", [A]) || A <- Args], ", ").

%% Run native binary
run_native_binary(BinaryPath, Opts, State) ->
    Timeout = case Opts#opts.timeout of
        undefined -> 10;
        T -> T
    end,

    case Opts#opts.arch of
        uefi ->
            run_uefi_qemu(BinaryPath, Timeout, Opts, State);
        _ ->
            case State#state.run_method of
                direct ->
                    run_direct(BinaryPath, Timeout);
                qemu ->
                    run_qemu_x86_64(BinaryPath, Timeout);
                qemu_aarch64 ->
                    run_qemu_aarch64(BinaryPath, Timeout);
                docker ->
                    DockerImage = case Opts#opts.docker_image of
                        undefined -> "thevlang/vlang:alpine";
                        I -> I
                    end,
                    run_docker(BinaryPath, Timeout, DockerImage)
            end
    end.

%% Run binary directly
run_direct(BinaryPath, Timeout) ->
    Cmd = io_lib:format("timeout ~p ~s 2>&1", [Timeout, BinaryPath]),
    Output = os:cmd(Cmd),
    case string:find(Output, "timed out") of
        nomatch -> {ok, Output};
        _ -> {error, timeout}
    end.

%% Run with QEMU x86_64 user-mode
run_qemu_x86_64(BinaryPath, Timeout) ->
    Cmd = io_lib:format("timeout ~p qemu-x86_64 ~s 2>&1", [Timeout, BinaryPath]),
    Output = os:cmd(Cmd),
    case string:find(Output, "timed out") of
        nomatch -> {ok, Output};
        _ -> {error, timeout}
    end.

%% Run with QEMU aarch64 user-mode
run_qemu_aarch64(BinaryPath, Timeout) ->
    Cmd = io_lib:format("timeout ~p qemu-aarch64 ~s 2>&1", [Timeout, BinaryPath]),
    Output = os:cmd(Cmd),
    case string:find(Output, "timed out") of
        nomatch -> {ok, Output};
        _ -> {error, timeout}
    end.

%% Run with Docker
run_docker(BinaryPath, Timeout, DockerImage) ->
    BinaryDir = filename:dirname(BinaryPath),
    BinaryName = filename:basename(BinaryPath),
    Cmd = io_lib:format("timeout ~p docker run --rm --platform linux/amd64 -v ~s:/work:ro ~s /work/~s 2>&1",
                        [Timeout, BinaryDir, DockerImage, BinaryName]),
    Output = os:cmd(Cmd),
    case string:find(Output, "timed out") of
        nomatch -> {ok, Output};
        _ -> {error, timeout}
    end.

%% Run UEFI binary in QEMU
run_uefi_qemu(PeBinaryPath, Timeout, Opts, _State) ->
    WorkDir = filename:dirname(PeBinaryPath),
    EfiDir = filename:join(WorkDir, "efi_root"),

    %% Create EFI boot structure
    EfiBootDir = filename:join([EfiDir, "EFI", "BOOT"]),
    filelib:ensure_dir(filename:join(EfiBootDir, "dummy")),
    file:copy(PeBinaryPath, filename:join(EfiBootDir, "BOOTX64.EFI")),

    %% Get OVMF path
    OvmfPath = case Opts#opts.ovmf_path of
        undefined -> "/opt/homebrew/share/qemu/edk2-x86_64-code.fd";
        P -> P
    end,

    %% Run QEMU with fat:rw: virtual FAT filesystem
    Cmd = io_lib:format(
        "timeout ~p qemu-system-x86_64 -bios ~s -drive format=raw,file=fat:rw:~s -serial stdio -nographic -no-reboot -monitor none -m 256M 2>/dev/null",
        [Timeout, OvmfPath, EfiDir]
    ),
    Output = os:cmd(Cmd),

    %% Clean up
    file:del_dir_r(EfiDir),

    %% Filter UEFI firmware noise
    Filtered = filter_uefi_output(Output),

    case string:find(Output, "timed out") of
        nomatch -> {ok, Filtered};
        _ -> {error, timeout}
    end.

%% Filter UEFI firmware noise from serial output
filter_uefi_output(Output) ->
    Lines = string:split(Output, "\n", all),
    Filtered = lists:filter(
        fun(Line) ->
            not (string:find(Line, "Shell>") =/= nomatch orelse
                 string:find(Line, "UEFI ") =/= nomatch orelse
                 string:find(Line, "map:") =/= nomatch orelse
                 string:find(Line, "FS") =/= nomatch orelse
                 string:find(Line, "BdsDxe:") =/= nomatch orelse
                 Line == "")
        end,
        Lines
    ),
    string:join(Filtered, "\n").

%% Compare outputs (normalize whitespace)
compare_output(Actual, Expected) ->
    NormActual = string:trim(Actual),
    NormExpected = string:trim(Expected),
    NormActual == NormExpected.

%% Print header
print_header(Arch, VCompiler, RunMethod, Platform, Timeout) ->
    io:format("============================================~n"),
    case Arch of
        x86_64 -> io:format("   V-to-Native x86_64 ELF Test Harness~n");
        arm64 -> io:format("     V-to-Native ARM64 Test Harness~n");
        uefi -> io:format("   V-to-Native x86_64 UEFI Test Harness~n")
    end,
    io:format("============================================~n~n"),
    io:format("V Compiler:  ~s~n", [VCompiler]),
    io:format("Platform:    ~p~n", [Platform]),
    io:format("Execution:   ~p~n", [RunMethod]),
    io:format("Timeout:     ~ps per test~n~n", [Timeout]).

%% Print summary
print_summary(State) ->
    TotalFailed = State#state.failed + State#state.compile_errors +
                  State#state.ir_errors + State#state.link_errors +
                  State#state.runtime_errors,

    io:format("~n============================================~n"),
    io:format("                Summary~n"),
    io:format("============================================~n~n"),
    io:format("Total:          ~p~n", [State#state.total]),
    log_color(?GREEN, io_lib:format("Passed:         ~p", [State#state.passed])),

    case TotalFailed > 0 of
        true ->
            log_color(?RED, io_lib:format("Failed:         ~p", [TotalFailed])),
            case State#state.compile_errors > 0 of
                true -> log_color(?YELLOW, io_lib:format("  - V compile:    ~p", [State#state.compile_errors]));
                false -> ok
            end,
            case State#state.ir_errors > 0 of
                true -> log_color(?YELLOW, io_lib:format("  - IR gen:       ~p", [State#state.ir_errors]));
                false -> ok
            end,
            case State#state.link_errors > 0 of
                true -> log_color(?YELLOW, io_lib:format("  - Link/assemble: ~p", [State#state.link_errors]));
                false -> ok
            end,
            case State#state.runtime_errors > 0 of
                true -> log_color(?YELLOW, io_lib:format("  - Runtime:      ~p", [State#state.runtime_errors]));
                false -> ok
            end,
            case State#state.failed > 0 of
                true -> log_color(?YELLOW, io_lib:format("  - Mismatch:     ~p", [State#state.failed]));
                false -> ok
            end;
        false -> ok
    end,

    io:format("~n"),

    %% Calculate pass rate
    case State#state.total > 0 of
        true ->
            PassRate = (State#state.passed * 100) div State#state.total,
            io:format("Pass rate: ~p%~n", [PassRate]);
        false ->
            io:format("No tests run~n")
    end,

    %% List failed tests
    case State#state.failed_tests of
        [] -> ok;
        Failed ->
            io:format("~nFailed tests:~n"),
            lists:foreach(fun(T) -> io:format("  - ~s~n", [T]) end, Failed)
    end,

    io:format("~n============================================~n").

%% Logging functions
log_info(Msg) ->
    io:format(?BLUE ++ "[INFO]" ++ ?NC ++ " ~s~n", [Msg]).

log_pass(Msg) ->
    io:format(?GREEN ++ "[PASS]" ++ ?NC ++ " ~s~n", [Msg]).

log_fail(Msg) ->
    io:format(?RED ++ "[FAIL]" ++ ?NC ++ " ~s~n", [Msg]).

log_warn(Msg) ->
    io:format(?YELLOW ++ "[WARN]" ++ ?NC ++ " ~s~n", [Msg]).

log_error(Msg) ->
    io:format(?RED ++ "[ERROR]" ++ ?NC ++ " ~s~n", [Msg]).

log_verbose(#opts{verbose = true}, Msg) ->
    io:format(?CYAN ++ "       " ++ ?NC ++ "~s~n", [Msg]);
log_verbose(_, _) ->
    ok.

log_color(Color, Msg) ->
    io:format(Color ++ "~s" ++ ?NC ++ "~n", [Msg]).
