-module(test_decode_verification).
-export([test/0]).

test() ->
    %% Parse the standalone parser's own BEAM file
    case vbeam_beam_standalone:parse_file("vbeam_beam_standalone.beam") of
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
            
            %% Count known vs unknown opcodes
            {Known, Unknown} = lists:foldl(fun
                ({{unknown_opcode, _}, _}, {K, U}) -> {K, U+1};
                (_, {K, U}) -> {K+1, U}
            end, {0, 0}, Instructions),
            
            io:format("  Known opcodes: ~p (~.1f%)~n", [Known, 100.0 * Known / (Known + Unknown)]),
            io:format("  Unknown opcodes: ~p (~.1f%)~n", [Unknown, 100.0 * Unknown / (Known + Unknown)]),
            
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error
    end.
