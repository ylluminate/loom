-module(test_interp_v2).
-export([test/0]).

%% Tests for vbeam_beam_interp_v2 edge cases

test() ->
    io:format("~n=== Interpreter V2 Edge Case Tests ===~n~n"),

    %% Run all tests
    Results = [
        test_atom_safety_nonexistent_function()
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
%% Test 1: Atom safety - non-existent function name
%%--------------------------------------------------------------------
test_atom_safety_nonexistent_function() ->
    io:format("Test 1: Atom safety for non-existent function... "),

    try
        %% Try to execute a function with a name that doesn't exist as an atom
        %% This should use list_to_existing_atom and return error, not create new atom
        NonExistentFunctionName = "this_function_definitely_does_not_exist_12345",

        %% Create a simple test module
        TestModule = simple_test_module,
        Code = [
            {function, test_func, 0, 1, [
                {label, 1},
                {func_info, TestModule, test_func, 0},
                {label, 2},
                {move, {integer, 42}, {x, 0}},
                return
            ]}
        ],

        %% Try to execute the non-existent function
        Result = vbeam_beam_interp_v2:execute({TestModule, Code},
                                               NonExistentFunctionName,
                                               [],
                                               []),

        %% Should return error, not crash with badarg from atom table exhaustion
        case Result of
            {error, {function_not_found, _, 0}} ->
                %% Verify the atom wasn't created
                AtomExists = try
                    _ = list_to_existing_atom(NonExistentFunctionName),
                    true
                catch
                    error:badarg -> false
                end,

                case AtomExists of
                    false ->
                        io:format("\e[32m✓ PASS\e[0m~n"),
                        pass;
                    true ->
                        io:format("\e[31m✗ FAIL\e[0m~n"),
                        io:format("  Atom was created when it shouldn't have been~n"),
                        fail
                end;
            Other ->
                io:format("\e[31m✗ FAIL\e[0m~n"),
                io:format("  Expected: {error, {function_not_found, ...}}~n"),
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
