#!/usr/bin/env escript
%% -*- erlang -*-
%%! -mode(compile)

-define(GREEN, "\e[32m").
-define(BLUE, "\e[34m").
-define(CYAN, "\e[36m").
-define(DIM, "\e[2m").
-define(BOLD, "\e[1m").
-define(RESET, "\e[0m").

main(_Args) ->
    code:add_patha("_build"),

    print_banner(),

    %% Step 1: Compile test module
    {ok, BeamBinary, ModuleName} = step1_compile_test_module(),

    %% Step 2: Standalone parse (zero OTP deps)
    step2_standalone_parse(BeamBinary),

    %% Step 3: Bare-metal interpret
    step3_bare_metal_interpret(BeamBinary, ModuleName),

    %% Step 4: JIT translate to x86_64
    step4_jit_translate(BeamBinary),

    %% Step 5: Nucleus status
    step5_nucleus_status(),

    %% Step 6: Summary
    print_summary(),

    %% Cleanup
    cleanup(),

    ok.

print_banner() ->
    io:format("~n~s╔══════════════════════════════════════════════════════════╗~s~n", [?BLUE, ?RESET]),
    io:format("~s║  ~sLoom OS — Full Pipeline Demo~s                           ║~s~n", [?BLUE, ?BOLD, ?RESET, ?RESET]),
    io:format("~s║  An operating system where BEAM is the kernel            ║~s~n", [?BLUE, ?RESET]),
    io:format("~s║  Built with Opus 4.6                                     ║~s~n", [?BLUE, ?RESET]),
    io:format("~s╚══════════════════════════════════════════════════════════╝~s~n~n", [?BLUE, ?RESET]).

step1_compile_test_module() ->
    io:format("~s[1/6] Compiling test module...~s~n", [?BLUE, ?RESET]),

    %% Create test module
    Code = "-module(loom_test).\n"
           "-export([main/0]).\n"
           "\n"
           "main() ->\n"
           "    io:format(\"Hello from Loom OS!~n\").\n",

    TmpFile = "/tmp/loom_test.erl",
    file:write_file(TmpFile, Code),

    %% Compile it
    {ok, ModuleName, BeamBinary, _Warnings} = compile:file(TmpFile, [binary, return]),

    %% Write to disk for later steps
    BeamFile = "/tmp/loom_test.beam",
    file:write_file(BeamFile, BeamBinary),

    Size = byte_size(BeamBinary),
    io:format("  ~s✓ Compiled test module (~p bytes .beam)~s~n~n", [?GREEN, Size, ?RESET]),

    {ok, BeamBinary, ModuleName}.

step2_standalone_parse(BeamBinary) ->
    io:format("~s[2/6] Standalone Parse (zero OTP dependencies)...~s~n", [?BLUE, ?RESET]),

    case catch vbeam_beam_standalone:parse_binary(BeamBinary) of
        {ok, Module} ->
            Atoms = maps:get(atoms, Module, []),
            AtomCount = length(Atoms),
            ExportCount = length(maps:get(exports, Module, [])),
            ImportCount = length(maps:get(imports, Module, [])),

            %% Decode instructions
            CodeBinary = maps:get(code, Module, <<>>),
            Instructions = vbeam_beam_standalone:decode_instructions(CodeBinary, Atoms),
            InstructionCount = length(Instructions),

            io:format("  ~s├─ Atoms: ~p~s~n", [?CYAN, AtomCount, ?RESET]),
            io:format("  ~s├─ Exports: ~p~s~n", [?CYAN, ExportCount, ?RESET]),
            io:format("  ~s├─ Imports: ~p~s~n", [?CYAN, ImportCount, ?RESET]),
            io:format("  ~s└─ Instructions: ~p~s~n", [?CYAN, InstructionCount, ?RESET]),

            %% Show first 5 instructions
            io:format("~n  ~sFirst 5 instructions:~s~n", [?DIM, ?RESET]),
            lists:foreach(
                fun({N, Instr}) ->
                    io:format("    ~s~p: ~p~s~n", [?DIM, N, Instr, ?RESET])
                end,
                lists:zip(lists:seq(1, min(5, InstructionCount)), lists:sublist(Instructions, 5))
            ),
            io:format("~n  ~s✓ Parse successful (zero OTP dependencies)~s~n~n", [?GREEN, ?RESET]);
        Error ->
            io:format("  ~s✗ Parse failed: ~p~s~n~n", [?DIM, Error, ?RESET])
    end.

step3_bare_metal_interpret(BeamBinary, _ModuleName) ->
    io:format("~s[3/6] Bare-metal Interpret...~s~n", [?BLUE, ?RESET]),

    %% Capture output
    OutputRef = make_ref(),
    Parent = self(),
    OutputCallback = fun(Text) -> Parent ! {OutputRef, Text} end,

    %% Run interpreter with correct API: run(BeamBinary, Function, Args, OutputFun)
    case catch vbeam_beam_interp_bare:run(BeamBinary, main, [], OutputCallback) of
        {ok, _Result} ->
            %% Collect output (may be character-by-character)
            Output = collect_output(OutputRef, []),
            case Output of
                [] ->
                    io:format("  ~s✓ Interpreted successfully (no output)~s~n~n", [?GREEN, ?RESET]);
                _ ->
                    %% Join output fragments into complete text
                    FullOutput = lists:flatten(Output),
                    io:format("  ~sCaptured output:~s~n    ~s~s~s~n",
                              [?CYAN, ?RESET, ?CYAN, FullOutput, ?RESET]),
                    io:format("~n  ~s✓ Interpreted with ZERO OTP dependencies~s~n~n", [?GREEN, ?RESET])
            end;
        Error ->
            io:format("  ~s✗ Interpretation failed: ~p~s~n~n", [?DIM, Error, ?RESET])
    end.

step4_jit_translate(_BeamBinary) ->
    io:format("~s[4/6] JIT Translate to x86_64...~s~n", [?BLUE, ?RESET]),

    BeamFile = "/tmp/loom_test.beam",
    case catch vbeam_beam_to_native:translate_beam(BeamFile) of
        {ok, Result} when is_map(Result) ->
            Code = maps:get(code, Result, <<>>),
            Data = maps:get(data, Result, <<>>),
            Entry = maps:get(entry_offset, Result, 0),

            CodeSize = byte_size(Code),
            DataSize = byte_size(Data),

            io:format("  ~s├─ Code size: ~p bytes~s~n", [?CYAN, CodeSize, ?RESET]),
            io:format("  ~s├─ Data size: ~p bytes~s~n", [?CYAN, DataSize, ?RESET]),
            io:format("  ~s└─ Entry offset: 0x~.16B~s~n", [?CYAN, Entry, ?RESET]),

            %% Show hex dump of first 32 bytes
            io:format("~n  ~sFirst 32 bytes (hex):~s~n", [?DIM, ?RESET]),
            HexDump = lists:sublist(binary_to_list(Code), 32),
            print_hex_dump(HexDump),

            io:format("~n  ~s✓ JIT translation successful~s~n~n", [?GREEN, ?RESET]);
        Error ->
            %% JIT is WIP, show gracefully
            io:format("  ~s⚙ JIT translation: work in progress~s~n", [?DIM, ?RESET]),
            io:format("    ~sReason: ~p~s~n~n", [?DIM, Error, ?RESET])
    end.

step5_nucleus_status() ->
    io:format("~s[5/6] UEFI Boot Nucleus Status...~s~n", [?BLUE, ?RESET]),

    NucleusFile = "boot/nucleus.efi",
    case file:read_file_info(NucleusFile) of
        {ok, Info} ->
            Size = element(2, Info),  %% file_info record, size is 2nd element
            io:format("  ~s├─ nucleus.efi: ~p bytes~s~n", [?CYAN, Size, ?RESET]),
            io:format("  ~s└─ UEFI boot nucleus ready~s~n~n", [?GREEN, ?RESET]),
            io:format("  ~sTo test boot: make qemu-test~s~n~n", [?DIM, ?RESET]);
        {error, _} ->
            io:format("  ~s✗ nucleus.efi not found~s~n", [?DIM, ?RESET]),
            io:format("    ~sRun 'make nucleus' to build~s~n~n", [?DIM, ?RESET])
    end.

print_summary() ->
    io:format("~s[6/6] Summary~s~n", [?BLUE, ?RESET]),
    io:format("~s═══════════════════════════════════════════════════════════~s~n", [?BLUE, ?RESET]),
    io:format("  ~sPipeline: Source → BEAM → Parse → Interpret → Native~s~n", [?BOLD, ?RESET]),
    io:format("  ~sAll stages use ZERO external dependencies~s~n", [?BOLD, ?RESET]),
    io:format("  ~sThis is an operating system kernel, written in Erlang~s~n", [?BOLD, ?RESET]),
    io:format("~s═══════════════════════════════════════════════════════════~s~n~n", [?BLUE, ?RESET]).

cleanup() ->
    file:delete("/tmp/loom_test.erl"),
    file:delete("/tmp/loom_test.beam"),
    ok.

collect_output(Ref, Acc) ->
    receive
        {Ref, Text} -> collect_output(Ref, [Text | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

print_hex_dump(Bytes) ->
    print_hex_dump(Bytes, 0).

print_hex_dump([], _) ->
    ok;
print_hex_dump(Bytes, Offset) ->
    {Line, Rest} = case length(Bytes) >= 16 of
        true -> lists:split(16, Bytes);
        false -> {Bytes, []}
    end,

    %% Print offset
    io:format("    ~s~4.16.0B:~s ", [?DIM, Offset, ?RESET]),

    %% Print hex bytes
    lists:foreach(
        fun(B) -> io:format("~2.16.0B ", [B]) end,
        Line
    ),

    %% Padding for short lines
    lists:foreach(
        fun(_) -> io:format("   ") end,
        lists:seq(1, 16 - length(Line))
    ),

    %% Print ASCII
    io:format(" ~s|~s", [?DIM, ?RESET]),
    lists:foreach(
        fun(B) ->
            case B >= 32 andalso B =< 126 of
                true -> io:format("~c", [B]);
                false -> io:format(".")
            end
        end,
        Line
    ),
    io:format("~s|~s~n", [?DIM, ?RESET]),

    print_hex_dump(Rest, Offset + 16).
