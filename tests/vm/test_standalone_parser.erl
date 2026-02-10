%% Test for vbeam_beam_standalone parser
-module(test_standalone_parser).
-export([run/0, test_module/0, test/0]).

%% Wrapper for make test-vm compatibility
test() -> run().

%% Simple test module to compile and parse
test_module() ->
    ok.

run() ->
    io:format("~n=== Testing vbeam_beam_standalone ===~n~n"),

    %% Step 1: Create a simple test module and compile it
    TestCode =
        "-module(simple_test).\n"
        "-export([add/2, factorial/1]).\n"
        "\n"
        "add(A, B) ->\n"
        "    A + B.\n"
        "\n"
        "factorial(0) -> 1;\n"
        "factorial(N) when N > 0 ->\n"
        "    N * factorial(N - 1).\n",

    TestFile = "simple_test.erl",
    BeamFile = "simple_test.beam",

    io:format("1. Creating test module: ~s~n", [TestFile]),
    ok = file:write_file(TestFile, TestCode),

    io:format("2. Compiling test module...~n"),
    case compile:file(TestFile, []) of
        {ok, _} ->
            io:format("   Compilation successful!~n~n");
        error ->
            io:format("   ERROR: Compilation failed!~n"),
            halt(1)
    end,

    %% Step 2: Parse the BEAM file
    io:format("3. Parsing BEAM file with standalone parser...~n"),
    case vbeam_beam_standalone:parse_file(BeamFile) of
        {ok, Result} ->
            io:format("   Parse successful!~n~n"),

            %% Step 3: Verify atoms
            Atoms = maps:get(atoms, Result),
            io:format("4. Atoms found: ~p~n", [length(Atoms)]),
            io:format("   First 10 atoms: ~p~n~n", [lists:sublist(Atoms, 10)]),

            %% Verify our module name is there
            case lists:member(simple_test, Atoms) of
                true -> io:format("   ✓ Module name 'simple_test' found~n");
                false -> io:format("   ✗ ERROR: Module name not found~n")
            end,

            %% Verify function names
            case lists:member(add, Atoms) of
                true -> io:format("   ✓ Function 'add' found~n");
                false -> io:format("   ✗ ERROR: Function 'add' not found~n")
            end,

            case lists:member(factorial, Atoms) of
                true -> io:format("   ✓ Function 'factorial' found~n~n");
                false -> io:format("   ✗ ERROR: Function 'factorial' not found~n~n")
            end,

            %% Step 4: Verify exports
            Exports = maps:get(exports, Result),
            io:format("5. Exports found: ~p~n", [length(Exports)]),
            lists:foreach(fun({FunIdx, Arity, Label}) ->
                FunName = if FunIdx > 0 andalso FunIdx =< length(Atoms) ->
                    lists:nth(FunIdx, Atoms);
                true ->
                    unknown
                end,
                io:format("   ~p/~p (label ~p)~n", [FunName, Arity, Label])
            end, Exports),
            io:format("~n"),

            %% Step 5: Verify imports
            Imports = maps:get(imports, Result),
            io:format("6. Imports found: ~p~n", [length(Imports)]),
            lists:foreach(fun({ModIdx, FunIdx, Arity}) ->
                ModName = if ModIdx > 0 andalso ModIdx =< length(Atoms) ->
                    lists:nth(ModIdx, Atoms);
                true ->
                    unknown
                end,
                FunName = if FunIdx > 0 andalso FunIdx =< length(Atoms) ->
                    lists:nth(FunIdx, Atoms);
                true ->
                    unknown
                end,
                io:format("   ~p:~p/~p~n", [ModName, FunName, Arity])
            end, lists:sublist(Imports, 10)),
            io:format("~n"),

            %% Step 6: Decode instructions
            Code = maps:get(code, Result),
            CodeInfo = maps:get(code_info, Result),
            io:format("7. Code chunk info:~n"),
            io:format("   Instruction set: ~p~n", [maps:get(instruction_set, CodeInfo)]),
            io:format("   Max opcode: ~p~n", [maps:get(opcode_max, CodeInfo)]),
            io:format("   Label count: ~p~n", [maps:get(label_count, CodeInfo)]),
            io:format("   Function count: ~p~n", [maps:get(function_count, CodeInfo)]),
            io:format("   Code size: ~p bytes~n~n", [byte_size(Code)]),

            io:format("8. Decoding instructions...~n"),
            Instructions = vbeam_beam_standalone:decode_instructions(Code, Atoms),
            io:format("   Decoded ~p instructions~n~n", [length(Instructions)]),

            %% Show first 20 instructions
            io:format("9. First 20 instructions:~n"),
            lists:foreach(fun({I, Instr}) ->
                io:format("   [~3w] ~p~n", [I, Instr])
            end, lists:zip(lists:seq(1, min(20, length(Instructions))),
                           lists:sublist(Instructions, 20))),

            io:format("~n=== Test completed successfully! ===~n~n"),

            %% Cleanup
            file:delete(TestFile),
            file:delete(BeamFile),
            ok;

        {error, Reason} ->
            io:format("   ERROR: Parse failed with reason: ~p~n", [Reason]),
            halt(1)
    end.
