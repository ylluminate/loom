#!/usr/bin/env escript
%% Analyze BEAM opcodes used in a .beam file or directory
%% Outputs the set of opcodes that need to be implemented

main([Path]) ->
    Files = case filelib:is_dir(Path) of
        true ->
            filelib:wildcard(filename:join(Path, "*.beam"));
        false ->
            [Path]
    end,

    io:format("Analyzing ~p BEAM files...~n", [length(Files)]),
    io:format("~n"),

    AllOpcodes = lists:foldl(
        fun(File, Acc) ->
            case beam_disasm:file(File) of
                {beam_file, Module, _, _, _, Code} ->
                    Opcodes = extract_opcodes(Code),
                    io:format("~p: ~p opcodes~n", [Module, sets:size(Opcodes)]),
                    sets:union(Acc, Opcodes);
                {error, _, Reason} ->
                    io:format("ERROR ~s: ~p~n", [File, Reason]),
                    Acc
            end
        end,
        sets:new(),
        Files
    ),

    io:format("~n"),
    io:format("=== OPCODE CLOSURE (~p unique opcodes) ===~n", [sets:size(AllOpcodes)]),
    io:format("~n"),

    %% Group opcodes by category
    OpList = lists:sort(sets:to_list(AllOpcodes)),

    ControlFlow = [Op || Op <- OpList, is_control_flow(Op)],
    DataOps = [Op || Op <- OpList, is_data_op(Op)],
    TypeTests = [Op || Op <- OpList, is_type_test(Op)],
    BifOps = [Op || Op <- OpList, is_bif_op(Op)],
    OtherOps = OpList -- (ControlFlow ++ DataOps ++ TypeTests ++ BifOps),

    io:format("Control Flow (~p):~n", [length(ControlFlow)]),
    [io:format("  - ~p~n", [Op]) || Op <- ControlFlow],
    io:format("~n"),

    io:format("Data Operations (~p):~n", [length(DataOps)]),
    [io:format("  - ~p~n", [Op]) || Op <- DataOps],
    io:format("~n"),

    io:format("Type Tests (~p):~n", [length(TypeTests)]),
    [io:format("  - ~p~n", [Op]) || Op <- TypeTests],
    io:format("~n"),

    io:format("BIF Operations (~p):~n", [length(BifOps)]),
    [io:format("  - ~p~n", [Op]) || Op <- BifOps],
    io:format("~n"),

    io:format("Other (~p):~n", [length(OtherOps)]),
    [io:format("  - ~p~n", [Op]) || Op <- OtherOps],

    halt(0);

main(_) ->
    io:format("Usage: analyze_opcodes.escript <beam_file_or_directory>~n"),
    io:format("~n"),
    io:format("Example:~n"),
    io:format("  analyze_opcodes.escript /tmp/hello.beam/~n"),
    halt(1).

%% Extract all opcode names from disassembled code
extract_opcodes(Code) ->
    lists:foldl(
        fun({function, _Name, _Arity, _Entry, Instructions}, Acc) ->
            lists:foldl(
                fun(Instr, AccInner) ->
                    case Instr of
                        {Op, _} when is_atom(Op) ->
                            sets:add_element(Op, AccInner);
                        {Op, _, _} when is_atom(Op) ->
                            sets:add_element(Op, AccInner);
                        {Op, _, _, _} when is_atom(Op) ->
                            sets:add_element(Op, AccInner);
                        {Op, _, _, _, _} when is_atom(Op) ->
                            sets:add_element(Op, AccInner);
                        {Op, _, _, _, _, _} when is_atom(Op) ->
                            sets:add_element(Op, AccInner);
                        _ ->
                            AccInner
                    end
                end,
                Acc,
                Instructions
            )
        end,
        sets:new(),
        Code
    ).

%% Categorization helpers
is_control_flow(label) -> true;
is_control_flow(func_info) -> true;
is_control_flow(return) -> true;
is_control_flow(jump) -> true;
is_control_flow(call) -> true;
is_control_flow(call_only) -> true;
is_control_flow(call_last) -> true;
is_control_flow(call_ext) -> true;
is_control_flow(call_ext_only) -> true;
is_control_flow(call_ext_last) -> true;
is_control_flow(call_fun) -> true;
is_control_flow(call_fun2) -> true;
is_control_flow(apply) -> true;
is_control_flow(apply_last) -> true;
is_control_flow(send) -> true;
is_control_flow('receive') -> true;
is_control_flow(wait) -> true;
is_control_flow(wait_timeout) -> true;
is_control_flow(_) -> false.

is_data_op(move) -> true;
is_data_op(put_list) -> true;
is_data_op(put_tuple) -> true;
is_data_op(put_tuple2) -> true;
is_data_op(put) -> true;
is_data_op(put_string) -> true;
is_data_op(get_list) -> true;
is_data_op(get_tuple_element) -> true;
is_data_op(get_hd) -> true;
is_data_op(get_tl) -> true;
is_data_op(set_tuple_element) -> true;
is_data_op(make_fun2) -> true;
is_data_op(make_fun3) -> true;
is_data_op(put_map_assoc) -> true;
is_data_op(put_map_exact) -> true;
is_data_op(get_map_elements) -> true;
is_data_op(_) -> false.

is_type_test(is_integer) -> true;
is_type_test(is_float) -> true;
is_type_test(is_number) -> true;
is_type_test(is_atom) -> true;
is_type_test(is_pid) -> true;
is_type_test(is_reference) -> true;
is_type_test(is_port) -> true;
is_type_test(is_nil) -> true;
is_type_test(is_binary) -> true;
is_type_test(is_list) -> true;
is_type_test(is_nonempty_list) -> true;
is_type_test(is_tuple) -> true;
is_type_test(is_boolean) -> true;
is_type_test(is_function) -> true;
is_type_test(is_function2) -> true;
is_type_test(is_map) -> true;
is_type_test(is_eq) -> true;
is_type_test(is_ne) -> true;
is_type_test(is_eq_exact) -> true;
is_type_test(is_ne_exact) -> true;
is_type_test(is_lt) -> true;
is_type_test(is_ge) -> true;
is_type_test(is_tagged_tuple) -> true;
is_type_test(test_arity) -> true;
is_type_test(_) -> false.

is_bif_op(gc_bif) -> true;
is_bif_op(bif) -> true;
is_bif_op(bif0) -> true;
is_bif_op(bif1) -> true;
is_bif_op(bif2) -> true;
is_bif_op(_) -> false.
