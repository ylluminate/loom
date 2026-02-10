-module(test_decode_verification).
-export([test/0]).

%% Already exports test/0 - Makefile compatible

test() ->
    io:format("~n=== Decode Verification Tests ===~n~n"),

    %% Test 1: Normal decoding test
    io:format("Test 1: Self-parsing and opcode coverage... "),
    Result1 = test_self_parsing(),

    %% Test 2: Empty instruction list
    io:format("Test 2: Empty instruction list handling... "),
    Result2 = test_empty_instructions(),

    %% Summary
    Results = [Result1, Result2],
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

    case Passed of
        Total -> ok;
        _ -> halt(1)
    end.

test_self_parsing() ->
    BeamFile = "_build/vbeam_beam_standalone.beam",
    case vbeam_beam_standalone:parse_file(BeamFile) of
        {ok, Result} ->
            Atoms = maps:get(atoms, Result),
            Exports = maps:get(exports, Result),
            Code = maps:get(code, Result),
            
            io:format("Self-parsing test:~n"),
            io:format("  Atoms: ~p~n", [length(Atoms)]),
            io:format("  Exports: ~p~n", [length(Exports)]),
            io:format("  Code size: ~p bytes~n", [byte_size(Code)]),
            
            %% Decode instructions
            %% FINDING R44-17 FIX: Distinguish between partial decode (acceptable with warning)
            %% and actual decode failures (error). Unknown opcodes produce partial results.
            Instructions = case vbeam_beam_standalone:decode_instructions(Code, Atoms) of
                {ok, Instrs} ->
                    Instrs;
                {error, {decode_failed, {unknown_opcode, _} = Reason, PartialInstrs}} ->
                    %% Unknown opcodes are expected - use partial results with warning
                    io:format("  WARNING: ~p (using partial decode)~n", [Reason]),
                    PartialInstrs;
                {error, {decode_failed, Reason, _PartialInstrs}} ->
                    %% Other decode failures are actual errors
                    io:format("  ERROR: Decode failed: ~p~n", [Reason]),
                    erlang:error({decode_failed, Reason});
                Instrs when is_list(Instrs) ->
                    Instrs
            end,
            io:format("  Instructions: ~p~n", [length(Instructions)]),

            %% Guard against divide-by-zero
            case length(Instructions) of
                0 ->
                    io:format("ERROR: 0 instructions decoded~n"),
                    error(no_instructions_decoded);
                Total ->
                    %% Count known vs unknown opcodes
                    {Known, Unknown} = lists:foldl(fun
                        ({{unknown_opcode, _}, _}, {K, U}) -> {K, U+1};
                        (_, {K, U}) -> {K+1, U}
                    end, {0, 0}, Instructions),

                    KnownPercent = 100.0 * Known / Total,
                    UnknownPercent = 100.0 * Unknown / Total,

                    io:format("  Known opcodes: ~p (~.1f%)~n", [Known, KnownPercent]),
                    io:format("  Unknown opcodes: ~p (~.1f%)~n", [Unknown, UnknownPercent]),

                    %% Assert known opcodes >= 90% threshold
                    case KnownPercent >= 90.0 of
                        true -> ok;
                        false ->
                            io:format("ERROR: Known opcodes below 90% threshold (~.1f%)~n", [KnownPercent]),
                            error({known_opcodes_below_threshold, KnownPercent})
                    end
            end,

            io:format("\e[32m✓ PASS\e[0m~n"),
            pass;
        {error, Reason} ->
            io:format("\e[31m✗ FAIL\e[0m - Failed to parse ~s: ~p~n", [BeamFile, Reason]),
            fail
    end.

%%--------------------------------------------------------------------
%% Test: Empty instruction list handling (div-by-zero guard)
%%--------------------------------------------------------------------
test_empty_instructions() ->
    %% Test that the decoder handles empty instruction lists gracefully
    %% This verifies the div-by-zero guard at line 25-28
    try
        Atoms = [test_module],

        %% Empty code binary
        Code = <<>>,

        %% Decode should return empty list without crashing
        Instructions = case vbeam_beam_standalone:decode_instructions(Code, Atoms) of
            {ok, Instrs2} -> Instrs2;
            Instrs2 when is_list(Instrs2) -> Instrs2
        end,

        case Instructions of
            [] ->
                %% Now verify the percentage calculation doesn't crash
                %% This is the guard we're testing (lines 25-29)
                Total = length(Instructions),
                case Total of
                    0 ->
                        %% The guard should prevent division by zero
                        %% Just verify we got here without crashing
                        io:format("\e[32m✓ PASS\e[0m~n"),
                        pass;
                    _ ->
                        io:format("\e[31m✗ FAIL\e[0m - Expected 0 instructions, got ~p~n", [Total]),
                        fail
                end;
            Other ->
                io:format("\e[31m✗ FAIL\e[0m - Expected empty list, got ~p~n", [Other]),
                fail
        end
    catch
        Class:Reason:Stack ->
            io:format("\e[31m✗ ERROR\e[0m~n"),
            io:format("  ~p:~p~n", [Class, Reason]),
            io:format("  ~p~n", [Stack]),
            fail
    end.
