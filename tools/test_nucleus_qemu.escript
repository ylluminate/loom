#!/usr/bin/env escript
%% -*- erlang -*-
%%! -mode(compile)

-mode(compile).
-compile({no_auto_import,[error/1]}).

%%
%% Test BEAM kernel nucleus in QEMU
%% Requires: qemu-system-x86_64, OVMF firmware (UEFI)
%%
%% Usage:
%%   ./tools/test_nucleus_qemu.escript          % Non-interactive (auto-verify)
%%   ./tools/test_nucleus_qemu.escript --interactive  % Interactive serial console
%%   ./tools/test_nucleus_qemu.escript -i
%%

main(Args) ->
    Interactive = lists:member("--interactive", Args) orelse lists:member("-i", Args),

    %% ANSI color codes
    put(color_reset, "\e[0m"),
    put(color_green, "\e[32m"),
    put(color_red, "\e[31m"),
    put(color_blue, "\e[34m"),
    put(color_cyan, "\e[36m"),

    %% Path setup
    ScriptPath = escript:script_name(),
    ToolsDir = filename:dirname(ScriptPath),
    ProjectRoot = filename:dirname(ToolsDir),
    BootDir = filename:join(ProjectRoot, "boot"),
    BuildDir = filename:join(ProjectRoot, "_build"),
    TmpBootDir = "/tmp/beam_os_boot",
    SerialLog = "/tmp/nucleus_serial.log",
    DiskImg = "/tmp/beam_os_boot.img",

    print_header("=== BEAM Kernel Nucleus Test ==="),
    io:format("~n"),

    try
        %% Step 1: Compile boot modules
        step("[1/5] Compiling boot modules..."),
        ok = compile_boot_modules(ProjectRoot, BuildDir),
        success("✓ Boot modules compiled"),

        %% Step 2: Build nucleus.efi
        step("[2/5] Building nucleus.efi..."),
        NucleusPath = filename:join(BootDir, "nucleus.efi"),
        ok = build_nucleus(ProjectRoot, BuildDir, NucleusPath),
        Size = filelib:file_size(NucleusPath),
        success(io_lib:format("✓ Built: nucleus.efi (~p bytes)", [Size])),
        io:format("~n"),

        %% Step 3: Create bootable disk image
        step("[3/5] Creating bootable disk image..."),
        ok = create_disk_image(NucleusPath, TmpBootDir, DiskImg),
        success(io_lib:format("✓ Disk image: ~s", [DiskImg])),
        io:format("~n"),

        %% Step 4: Locate OVMF firmware
        step("[4/5] Locating UEFI firmware..."),
        {OvmfCode, OvmfVars} = find_ovmf_firmware(),
        success(io_lib:format("✓ UEFI firmware: ~s", [OvmfCode])),
        io:format("~n"),

        %% Step 5: Launch QEMU
        step("[5/5] Launching QEMU..."),
        print_separator(),

        case Interactive of
            true ->
                launch_qemu_interactive(OvmfCode, OvmfVars, DiskImg);
            false ->
                launch_qemu_verify(OvmfCode, OvmfVars, DiskImg, SerialLog)
        end
    catch
        throw:{error, Reason} ->
            error(io_lib:format("✗ ~s", [Reason])),
            halt(1);
        Class:Error:Stack ->
            error(io_lib:format("✗ Unexpected error: ~p:~p", [Class, Error])),
            io:format("~p~n", [Stack]),
            halt(1)
    end.

%%
%% Compilation
%%

compile_boot_modules(ProjectRoot, BuildDir) ->
    %% Create build directory
    ok = filelib:ensure_dir(filename:join(BuildDir, "dummy")),

    %% Compile PE emitter
    PeEmitterPath = filename:join([ProjectRoot, "arch", "formats", "vbeam_native_pe.erl"]),
    case os:cmd(io_lib:format("erlc -o ~s ~s 2>&1", [BuildDir, PeEmitterPath])) of
        "" -> ok;
        PeOutput ->
            case string:str(PeOutput, "error") of
                0 -> ok;
                _ -> throw({error, io_lib:format("PE emitter compile failed: ~s", [PeOutput])})
            end
    end,

    %% Compile font modules
    FontDir = filename:join([ProjectRoot, "boot", "fonts"]),
    FontFiles = filelib:wildcard(filename:join(FontDir, "vbeam_font_*.erl")),
    lists:foreach(fun(FontFile) ->
        Cmd = io_lib:format("erlc -o ~s ~s 2>&1", [BuildDir, FontFile]),
        case os:cmd(Cmd) of
            "" -> ok;
            Output ->
                case string:str(Output, "error") of
                    0 -> ok;
                    _ -> throw({error, io_lib:format("Font compile failed: ~s", [Output])})
                end
        end
    end, FontFiles),

    %% Compile nucleus boot module
    NucleusBootPath = filename:join([ProjectRoot, "boot", "vbeam_nucleus_boot.erl"]),
    case os:cmd(io_lib:format("erlc -o ~s ~s 2>&1", [BuildDir, NucleusBootPath])) of
        "" -> ok;
        NOutput ->
            case string:str(NOutput, "error") of
                0 -> ok;
                _ -> throw({error, io_lib:format("Nucleus boot compile failed: ~s", [NOutput])})
            end
    end,

    ok.

build_nucleus(_ProjectRoot, BuildDir, OutputPath) ->
    Cmd = io_lib:format(
        "erl -noshell -pa ~s "
        "-eval \"vbeam_nucleus_boot:build(\\\"~s\\\")\" "
        "-s init stop 2>&1",
        [BuildDir, OutputPath]
    ),
    Output = os:cmd(Cmd),

    case filelib:is_file(OutputPath) of
        true -> ok;
        false ->
            throw({error, io_lib:format("Build failed - nucleus.efi not found. Output: ~s", [Output])})
    end.

%%
%% Disk Image Creation
%%

create_disk_image(NucleusPath, TmpBootDir, DiskImg) ->
    %% Clean up old artifacts
    os:cmd(io_lib:format("rm -rf ~s", [TmpBootDir])),
    os:cmd("rm -f /tmp/beam_os_boot_raw.dmg /tmp/beam_os_boot_rw.dmg /tmp/beam_os_boot_final.cdr " ++ DiskImg),

    %% Create EFI boot structure
    EfiBootDir = filename:join([TmpBootDir, "EFI", "BOOT"]),
    ok = filelib:ensure_dir(filename:join(EfiBootDir, "dummy")),

    {ok, _} = file:copy(NucleusPath, filename:join(EfiBootDir, "BOOTX64.EFI")),

    StartupNsh = filename:join(TmpBootDir, "startup.nsh"),
    ok = file:write_file(StartupNsh, <<"FS0:\\EFI\\BOOT\\BOOTX64.EFI\n">>),

    %% Create FAT32 disk image (platform-specific)
    case os:find_executable("hdiutil") of
        false ->
            create_disk_linux(EfiBootDir, StartupNsh, DiskImg);
        _HdiutilPath ->
            create_disk_macos(EfiBootDir, StartupNsh, DiskImg)
    end,

    ok.

create_disk_macos(EfiBootDir, StartupNsh, DiskImg) ->
    %% macOS: use hdiutil
    os:cmd("hdiutil create -size 33m -fs FAT32 -volname BEAMOS -layout NONE -o /tmp/beam_os_boot_raw >/dev/null 2>&1"),
    os:cmd("hdiutil convert /tmp/beam_os_boot_raw.dmg -format UDRW -o /tmp/beam_os_boot_rw >/dev/null 2>&1"),
    os:cmd("hdiutil attach /tmp/beam_os_boot_rw.dmg -nobrowse -mountpoint /tmp/beam_os_mnt >/dev/null 2>&1"),

    os:cmd("mkdir -p /tmp/beam_os_mnt/EFI/BOOT"),
    os:cmd(io_lib:format("cp ~s/BOOTX64.EFI /tmp/beam_os_mnt/EFI/BOOT/", [EfiBootDir])),
    os:cmd(io_lib:format("cp ~s /tmp/beam_os_mnt/", [StartupNsh])),

    os:cmd("hdiutil detach /tmp/beam_os_mnt >/dev/null 2>&1"),
    os:cmd("hdiutil convert /tmp/beam_os_boot_rw.dmg -format UDTO -o /tmp/beam_os_boot_final >/dev/null 2>&1"),
    os:cmd(io_lib:format("mv /tmp/beam_os_boot_final.cdr ~s", [DiskImg])),
    os:cmd("rm -f /tmp/beam_os_boot_raw.dmg /tmp/beam_os_boot_rw.dmg"),
    ok.

create_disk_linux(EfiBootDir, StartupNsh, DiskImg) ->
    %% Linux: use dd + mtools
    os:cmd(io_lib:format("dd if=/dev/zero of=~s bs=1M count=33 2>/dev/null", [DiskImg])),
    os:cmd(io_lib:format("mkfs.fat -F 32 ~s >/dev/null 2>&1", [DiskImg])),
    os:cmd(io_lib:format("mmd -i ~s ::EFI ::EFI/BOOT", [DiskImg])),
    os:cmd(io_lib:format("mcopy -i ~s ~s/BOOTX64.EFI ::EFI/BOOT/", [DiskImg, EfiBootDir])),
    os:cmd(io_lib:format("mcopy -i ~s ~s ::", [DiskImg, StartupNsh])),
    ok.

%%
%% OVMF Firmware Location
%%

find_ovmf_firmware() ->
    OvmfCodePaths = [
        "/opt/homebrew/share/qemu/edk2-x86_64-code.fd",
        "/opt/homebrew/Cellar/qemu/*/share/qemu/edk2-x86_64-code.fd",
        "/usr/share/qemu/OVMF.fd",
        "/usr/share/OVMF/OVMF_CODE.fd",
        "/usr/share/edk2/ovmf/OVMF_CODE.fd"
    ],

    OvmfVarsPaths = [
        "/opt/homebrew/share/qemu/edk2-i386-vars.fd",
        "/usr/share/OVMF/OVMF_VARS.fd",
        "/usr/share/edk2/ovmf/OVMF_VARS.fd"
    ],

    OvmfCode = case find_first_file(OvmfCodePaths) of
        {ok, CodePath} -> CodePath;
        error ->
            throw({error, "OVMF firmware not found. Install with: brew install qemu (macOS) or apt install ovmf (Linux)"})
    end,

    OvmfVarsSrc = case find_first_file(OvmfVarsPaths) of
        {ok, VarsPath} -> VarsPath;
        error -> OvmfCode  %% Fallback to code fd
    end,

    %% Create writable vars copy
    OvmfVars = "/tmp/ovmf_vars.fd",
    {ok, _} = file:copy(OvmfVarsSrc, OvmfVars),

    {OvmfCode, OvmfVars}.

find_first_file([]) ->
    error;
find_first_file([Pattern | Rest]) ->
    %% Expand glob patterns
    Files = filelib:wildcard(Pattern),
    case Files of
        [] ->
            find_first_file(Rest);
        [File | _] ->
            case filelib:is_file(File) of
                true -> {ok, File};
                false -> find_first_file(Rest)
            end
    end.

%%
%% QEMU Launch
%%

launch_qemu_interactive(OvmfCode, OvmfVars, DiskImg) ->
    io:format("Interactive mode — Ctrl+A, X to exit~n"),
    print_separator(),
    io:format("~n"),

    Cmd = io_lib:format(
        "qemu-system-x86_64 "
        "-machine q35 "
        "-m 256M "
        "-drive if=pflash,format=raw,readonly=on,file=~s "
        "-drive if=pflash,format=raw,file=~s "
        "-drive format=raw,file=~s "
        "-nographic "
        "-no-reboot",
        [OvmfCode, OvmfVars, DiskImg]
    ),

    os:cmd(Cmd),
    halt(0).

launch_qemu_verify(OvmfCode, OvmfVars, DiskImg, SerialLog) ->
    %% Clean old log
    file:delete(SerialLog),

    io:format("Non-interactive mode — 10s timeout, serial → ~s~n", [SerialLog]),
    print_separator(),

    Cmd = io_lib:format(
        "timeout 10 qemu-system-x86_64 "
        "-machine q35 "
        "-m 256M "
        "-drive if=pflash,format=raw,readonly=on,file=~s "
        "-drive if=pflash,format=raw,file=~s "
        "-drive format=raw,file=~s "
        "-display none "
        "-serial file:~s "
        "-no-reboot "
        "2>/dev/null || true",
        [OvmfCode, OvmfVars, DiskImg, SerialLog]
    ),

    os:cmd(Cmd),

    io:format("~n"),
    io:format("=== Serial Output ===~n"),

    case file:read_file(SerialLog) of
        {ok, LogBin} ->
            %% Extract printable strings
            LogStr = binary_to_list(LogBin),
            PrintableLines = extract_printable_lines(LogStr),
            lists:foreach(fun(Line) ->
                io:format("~s~n", [Line])
            end, PrintableLines),
            io:format("~n"),

            %% Verify expected output
            case string:str(LogStr, "Loom Kernel") of
                0 ->
                    error("✗ FAIL: Expected 'Loom Kernel' in serial output"),
                    io:format("  Raw log: ~s~n", [SerialLog]),
                    halt(1);
                _ ->
                    success("✓ PASS: Nucleus booted and printed to serial"),
                    case string:str(LogStr, "HAL") of
                        0 -> ok;
                        _ -> success("✓ PASS: HAL initialization confirmed")
                    end,
                    halt(0)
            end;
        {error, _} ->
            error("✗ FAIL: No serial output captured"),
            halt(1)
    end.

extract_printable_lines(Str) ->
    %% Split on newlines, filter out ANSI escapes and empty lines
    Lines = string:tokens(Str, "\n\r"),
    lists:filter(fun(Line) ->
        Stripped = strip_ansi(Line),
        Trimmed = string:strip(Stripped),
        Trimmed /= "" andalso not string:str(Trimmed, "[") == 1
    end, Lines).

strip_ansi(Str) ->
    %% Remove ANSI escape sequences
    re:replace(Str, "\e\\[[0-9;]*m", "", [global, {return, list}]).

%%
%% Utility Functions
%%

print_header(Msg) ->
    io:format("~s~s~s~n", [get(color_cyan), Msg, get(color_reset)]).

step(Msg) ->
    io:format("~s~s~s~n", [get(color_blue), Msg, get(color_reset)]).

success(Msg) ->
    io:format("~s~s~s~n", [get(color_green), Msg, get(color_reset)]).

error(Msg) ->
    io:format("~s~s~s~n", [get(color_red), Msg, get(color_reset)]).

print_separator() ->
    io:format("────────────────────────────────────────~n").
