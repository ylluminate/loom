#!/usr/bin/env escript
%% BEAM Bytecode Runner
%% Loads and executes a .beam file using the minimal interpreter

main([BeamFile, FunctionName | Args]) ->
    %% Parse the BEAM file
    case vbeam_beam_parser:parse_file(BeamFile) of
        {ok, Chunks} ->
            io:format("Loaded ~s~n", [BeamFile]),
            io:format("Chunks: ~p~n", [maps:keys(Chunks)]),

            %% Extract atom table for display
            Atoms = case maps:get('AtU8', Chunks, undefined) of
                undefined ->
                    case maps:get('Atom', Chunks, undefined) of
                        undefined -> [];
                        {AtomList, _} -> AtomList
                    end;
                {AtomList, _} -> AtomList
            end,

            io:format("Atoms: ~p~n", [Atoms]),

            %% Extract exports for display
            Exports = maps:get('ExpT', Chunks, []),
            io:format("Exports: ~p~n", [Exports]),

            %% Execute the function
            case vbeam_beam_interp:execute(Chunks, FunctionName, Args) of
                {ok, Result} ->
                    io:format("~nResult: ~p~n", [Result]),
                    halt(0);
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    halt(1)
            end;

        {error, Reason} ->
            io:format("Failed to parse BEAM file: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    usage().

usage() ->
    io:format("Usage: vbeam_beam_run.escript <beam_file> <function_name> [args...]~n"),
    io:format("~n"),
    io:format("Example:~n"),
    io:format("  vbeam_beam_run.escript /tmp/test.beam/v.main.beam main~n"),
    halt(1).
