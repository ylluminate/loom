#!/usr/bin/env escript
%% BEAM Bytecode Runner v2
%% Uses beam_disasm for instruction decoding

-mode(compile).

main([BeamFile, FunctionName | Args]) ->
    %% Add current directory to code path
    ScriptDir = filename:dirname(escript:script_name()),
    code:add_patha(ScriptDir),
    io:format("Loading ~s...~n", [BeamFile]),

    %% Disassemble the BEAM file
    case beam_disasm:file(BeamFile) of
        {beam_file, Module, Exports, _Attrs, _CompileInfo, Code} ->
            io:format("Module: ~p~n", [Module]),
            io:format("Exports: ~p~n", [Exports]),
            io:format("Functions: ~p~n", [[{Name, Arity} || {function, Name, Arity, _, _} <- Code]]),
            io:format("~n"),

            %% Convert string args to appropriate types (try integer, fallback to atom/string)
            ConvertedArgs = lists:map(
                fun(Arg) ->
                    case string:to_integer(Arg) of
                        {Int, []} -> Int;
                        _ ->
                            %% Try as atom
                            try list_to_existing_atom(Arg)
                            catch _:_ -> list_to_binary(Arg)
                            end
                    end
                end,
                Args
            ),

            io:format("Executing ~p(~p)...~n", [FunctionName, ConvertedArgs]),
            io:format("~n"),

            %% Execute the function
            case vbeam_beam_interp_v2:execute({Module, Code}, FunctionName, ConvertedArgs) of
                {ok, Result} ->
                    io:format("~n==> Result: ~p~n", [Result]),
                    halt(0);
                {error, Reason} ->
                    io:format("~n==> Error: ~p~n", [Reason]),
                    halt(1)
            end;

        {error, beam_lib, Reason} ->
            io:format("Failed to disassemble BEAM file: ~p~n", [Reason]),
            halt(1);

        {error, Reason} ->
            io:format("Failed to load BEAM file: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    usage().

usage() ->
    io:format("Usage: vbeam_beam_run_v2.escript <beam_file> <function_name> [args...]~n"),
    io:format("~n"),
    io:format("Example:~n"),
    io:format("  vbeam_beam_run_v2.escript /tmp/test_simple.beam add 5 3~n"),
    io:format("  vbeam_beam_run_v2.escript /tmp/test.beam/v.main.beam main~n"),
    halt(1).
