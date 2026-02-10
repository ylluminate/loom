-module(test_bare_pipeline).
-export([test/0]).

%% Already exports test/0 - Makefile compatible

test() ->
    io:format("~n=== Bare-Metal Pipeline Integration Tests ===~n~n"),

    %% Ensure output directory exists
    filelib:ensure_dir("_build/dummy"),

    %% Run all tests
    Results = [
        test_simple_return(),
        test_io_output_capture(),
        test_standalone_parser()
    ],

    %% Print summary
    Passed = length([R || R <- Results, R =:= pass]),
    Total = length(Results),

    io:format("~n~s~n", [string:copies("=", 50)]),
    case Passed of
        Total ->
            io:format("\e[32m✓ All ~p/~p tests passed\e[0m~n", [Passed, Total]);
        _ ->
            io:format("\e[31m✗ ~p/~p tests passed (~p failed)\e[0m~n",
                     [Passed, Total, Total - Passed])
    end,
    io:format("~s~n~n", [string:copies("=", 50)]),

    %% Return success/failure
    case Passed of
        Total -> ok;
        _ -> halt(1)
    end.

%%--------------------------------------------------------------------
%% Test 1: Simple return value
%%--------------------------------------------------------------------
test_simple_return() ->
    io:format("Test 1: Simple return value... "),

    try
        %% Compile the test file
        SrcFile = "tests/data/loom_hello.erl",
        case compile:file(SrcFile, [{outdir, "_build"}, return_errors]) of
            {ok, _} -> ok;
            error -> throw({compile_error, SrcFile});
            {error, Errors, _} -> throw({compile_error, Errors})
        end,

        %% Read .beam binary
        BeamFile = "_build/loom_hello.beam",
        {ok, BeamBin} = file:read_file(BeamFile),

        %% Parse with standalone parser
        {ok, _ParsedBeam} = vbeam_beam_standalone:parse_binary(BeamBin),

        %% Create output collector
        put(test_output, ""),
        OutputFun = fun(Str) ->
            Old = get(test_output),
            put(test_output, Old ++ Str),
            ok
        end,

        %% Run with bare-metal interpreter
        Result = vbeam_beam_interp_bare:run(BeamBin, OutputFun),

        %% Verify return value
        case Result of
            {ok, 'Hello from V-on-BEAM on Loom!'} ->
                io:format("\e[32m✓ PASS\e[0m~n"),
                pass;
            Other ->
                io:format("\e[31m✗ FAIL\e[0m~n"),
                io:format("  Expected: {ok, 'Hello from V-on-BEAM on Loom!'}~n"),
                io:format("  Got: ~p~n", [Other]),
                fail
        end
    catch
        Class:Reason:Stack ->
            io:format("\e[31m✗ ERROR\e[0m~n"),
            io:format("  ~p:~p~n", [Class, Reason]),
            io:format("  ~p~n", [Stack]),
            fail
    end.

%%--------------------------------------------------------------------
%% Test 2: IO output capture
%%--------------------------------------------------------------------
test_io_output_capture() ->
    io:format("Test 2: IO output capture... "),

    try
        %% Compile the test file
        SrcFile = "tests/data/loom_hello_print.erl",
        case compile:file(SrcFile, [{outdir, "_build"}, return_errors]) of
            {ok, _} -> ok;
            error -> throw({compile_error, SrcFile});
            {error, Errors, _} -> throw({compile_error, Errors})
        end,

        %% Read .beam binary
        BeamFile = "_build/loom_hello_print.beam",
        {ok, BeamBin} = file:read_file(BeamFile),

        %% Parse with standalone parser
        {ok, _ParsedBeam} = vbeam_beam_standalone:parse_binary(BeamBin),

        %% Create output collector
        put(test_output, ""),
        OutputFun = fun(Str) ->
            Old = get(test_output),
            put(test_output, Old ++ Str),
            ok
        end,

        %% Run with bare-metal interpreter
        Result = vbeam_beam_interp_bare:run(BeamBin, OutputFun),

        %% Get captured output
        Output = get(test_output),

        %% Verify output contains expected string
        ExpectedStr = "Hello from V-on-BEAM on Loom!",
        OutputOk = case string:find(Output, ExpectedStr) of
            nomatch -> false;
            _ -> true
        end,

        %% Verify return value
        ReturnOk = case Result of
            {ok, ok} -> true;
            _ -> false
        end,

        case OutputOk andalso ReturnOk of
            true ->
                io:format("\e[32m✓ PASS\e[0m~n"),
                pass;
            false ->
                io:format("\e[31m✗ FAIL\e[0m~n"),
                if
                    not OutputOk ->
                        io:format("  Output check failed~n"),
                        io:format("  Expected substring: ~p~n", [ExpectedStr]),
                        io:format("  Got: ~p~n", [Output]);
                    true -> ok
                end,
                if
                    not ReturnOk ->
                        io:format("  Return value check failed~n"),
                        io:format("  Expected: {ok, ok}~n"),
                        io:format("  Got: ~p~n", [Result]);
                    true -> ok
                end,
                fail
        end
    catch
        Class:Reason:Stack ->
            io:format("\e[31m✗ ERROR\e[0m~n"),
            io:format("  ~p:~p~n", [Class, Reason]),
            io:format("  ~p~n", [Stack]),
            fail
    end.

%%--------------------------------------------------------------------
%% Test 3: Standalone parser verification
%%--------------------------------------------------------------------
test_standalone_parser() ->
    io:format("Test 3: Standalone parser verification... "),

    try
        %% Use already compiled file
        BeamFile = "_build/loom_hello.beam",
        {ok, BeamBin} = file:read_file(BeamFile),

        %% Parse with standalone parser
        {ok, ParsedBeam} = vbeam_beam_standalone:parse_binary(BeamBin),

        %% Verify atoms list contains module name
        Atoms = maps:get(atoms, ParsedBeam),
        ModuleName = loom_hello,
        AtomsOk = lists:member(ModuleName, Atoms),

        %% Verify exports are non-empty
        Exports = maps:get(exports, ParsedBeam),
        ExportsOk = length(Exports) > 0,

        %% Verify code binary is non-empty
        Code = maps:get(code, ParsedBeam),
        CodeOk = byte_size(Code) > 0,

        %% Decode instructions
        Instructions = vbeam_beam_standalone:decode_instructions(Code, Atoms),
        InstsOk = is_list(Instructions) andalso length(Instructions) > 0,

        AllOk = AtomsOk andalso ExportsOk andalso CodeOk andalso InstsOk,

        case AllOk of
            true ->
                io:format("\e[32m✓ PASS\e[0m~n"),
                pass;
            false ->
                io:format("\e[31m✗ FAIL\e[0m~n"),
                if
                    not AtomsOk ->
                        io:format("  Module name not in atoms list~n"),
                        io:format("  Expected: ~p~n", [ModuleName]),
                        io:format("  Atoms: ~p~n", [Atoms]);
                    true -> ok
                end,
                if
                    not ExportsOk ->
                        io:format("  Exports list is empty~n");
                    true -> ok
                end,
                if
                    not CodeOk ->
                        io:format("  Code binary is empty~n");
                    true -> ok
                end,
                if
                    not InstsOk ->
                        io:format("  Decoded instructions is empty~n");
                    true -> ok
                end,
                fail
        end
    catch
        Class:Reason:Stack ->
            io:format("\e[31m✗ ERROR\e[0m~n"),
            io:format("  ~p:~p~n", [Class, Reason]),
            io:format("  ~p~n", [Stack]),
            fail
    end.
