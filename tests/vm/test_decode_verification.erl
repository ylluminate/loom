-module(test_decode_verification).
-export([test/0]).

%% Already exports test/0 - Makefile compatible

test() ->
    %% Parse the standalone parser's own BEAM file
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
            Instructions = vbeam_beam_standalone:decode_instructions(Code, Atoms),
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

            ok;
        {error, Reason} ->
            io:format("ERROR: Failed to parse ~s: ~p~n", [BeamFile, Reason]),
            error  %% This will now cause halt(1) via Makefile
    end.
