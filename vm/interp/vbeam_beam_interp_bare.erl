%% BEAM Bytecode Interpreter - Bare Metal Edition
%% NO OTP dependencies - self-contained for nucleus.efi embedding
%%
%% Design:
%% - Uses vbeam_beam_standalone for loading (no beam_disasm)
%% - Output via callback function (no io:format)
%% - Explicit BIF dispatch table (no apply/3 fallback)
%% - Inline list operations (no lists:* imports)
%% - State is a map (simpler than records on bare metal)

-module(vbeam_beam_interp_bare).
-export([run/2, run/4, load_module/1]).

%% ============================================================================
%% Public API
%% ============================================================================

%% Run module:main/0 from a .beam binary
%% OutputFun: fun((String) -> ok) - callback for output
-spec run(binary(), fun((string()) -> ok)) -> {ok, term()} | {error, term()}.
run(BeamBinary, OutputFun) ->
    run(BeamBinary, main, [], OutputFun).

%% Run module:Function(Args) from a .beam binary
-spec run(binary(), atom(), [term()], fun((string()) -> ok)) -> {ok, term()} | {error, term()}.
run(BeamBinary, Function, Args, OutputFun) ->
    case load_module(BeamBinary) of
        {ok, Module} ->
            Arity = length_bare(Args),
            %% Convert function atom to binary to match parser format
            FunctionBin = atom_to_binary(Function, utf8),
            case get_function_code(Module, FunctionBin, Arity) of
                {ok, Instructions} ->
                    State = init_state(Module, FunctionBin, Arity, Args, Instructions, OutputFun),
                    execute(State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Load a module from .beam binary
-spec load_module(binary()) -> {ok, map()} | {error, term()}.
load_module(BeamBinary) ->
    case vbeam_beam_standalone:parse_binary(BeamBinary) of
        {ok, Parsed} ->
            %% Decode instructions from Code chunk
            Atoms = get_atoms(Parsed),
            CodeBinary = maps:get(code, Parsed, <<>>),
            Instructions = case vbeam_beam_standalone:decode_instructions(CodeBinary, Atoms) of
                {ok, Instrs} -> Instrs;
                {error, {decode_failed, _Reason, PartialInstrs}} -> PartialInstrs;
                Instrs when is_list(Instrs) -> Instrs
            end,

            %% Build function map and label map
            {Functions, Labels} = build_maps(Instructions, Atoms, Parsed),

            {ok, #{
                module_name => get_module_name(Atoms, Parsed),
                atoms => Atoms,
                literals => maps:get(literals, Parsed, []),
                strings => maps:get(strings, Parsed, <<>>),
                imports => maps:get(imports, Parsed, []),
                exports => maps:get(exports, Parsed, []),
                functions => Functions,
                labels => Labels
            }};
        {error, Reason} ->
            {error, {parse_failed, Reason}}
    end.

%% ============================================================================
%% Module Loading Helpers
%% ============================================================================

get_atoms(Parsed) ->
    maps:get(atoms, Parsed, []).

get_module_name(Atoms, _Parsed) ->
    %% Module name is typically atom 1 (index 1, 0-based would be atom 0)
    case Atoms of
        [ModName | _] -> ModName;
        [] -> unknown_module
    end.

%% Build function map and label map from instruction stream
%% The instruction stream is flat - we need to split by func_info markers
build_maps(Instructions, Atoms, Parsed) ->
    Imports = maps:get(imports, Parsed, []),
    split_functions(Instructions, Atoms, Imports, #{}, #{}, undefined, [], 0).

%% Split flat instruction stream into functions and build label map
split_functions([], _Atoms, _Imports, FuncAcc, LabelAcc, CurrentFun, CurrentInstrs, _PCOffset) ->
    %% Finalize last function
    NewFuncAcc = case CurrentFun of
        undefined -> FuncAcc;
        _ -> FuncAcc#{CurrentFun => reverse_bare(CurrentInstrs)}
    end,
    {NewFuncAcc, LabelAcc};

split_functions([{func_info, [{atom, _Mod}, {atom, Fun}, {integer, Arity}]} | Rest],
                Atoms, Imports, FuncAcc, LabelAcc, PrevFun, PrevInstrs, PCOffset) ->
    %% Finalize previous function if any
    NewFuncAcc = case PrevFun of
        undefined -> FuncAcc;
        _ -> FuncAcc#{PrevFun => reverse_bare(PrevInstrs)}
    end,
    %% Start new function
    NewFun = {Fun, Arity},
    NewInstrs = [{func_info, _Mod, Fun, Arity}],
    split_functions(Rest, Atoms, Imports, NewFuncAcc, LabelAcc, NewFun, NewInstrs, PCOffset + 1);

split_functions([{label, [{integer, L}]} | Rest], Atoms, Imports,
                FuncAcc, LabelAcc, CurrentFun, CurrentInstrs, PCOffset) ->
    %% Label - record its position within current function
    InstrIdx = length_bare(CurrentInstrs),  % Current position in this function
    NewLabelAcc = case CurrentFun of
        undefined -> LabelAcc;
        _ -> LabelAcc#{L => {CurrentFun, InstrIdx}}
    end,
    NewInstrs = [{ label, L} | CurrentInstrs],  % Prepend to build in reverse
    split_functions(Rest, Atoms, Imports, FuncAcc, NewLabelAcc,
                    CurrentFun, NewInstrs, PCOffset + 1);

split_functions([{return, []} | Rest], Atoms, Imports,
                FuncAcc, LabelAcc, CurrentFun, CurrentInstrs, PCOffset) ->
    %% Return often marks end of function
    NewInstrs = [return | CurrentInstrs],
    split_functions(Rest, Atoms, Imports, FuncAcc, LabelAcc,
                    CurrentFun, NewInstrs, PCOffset + 1);

split_functions([{int_code_end, []} | Rest], Atoms, Imports,
                FuncAcc, LabelAcc, CurrentFun, CurrentInstrs, PCOffset) ->
    %% End marker - could be end of function or end of module
    NewInstrs = [{int_code_end} | CurrentInstrs],
    split_functions(Rest, Atoms, Imports, FuncAcc, LabelAcc,
                    CurrentFun, NewInstrs, PCOffset + 1);

split_functions([Instr | Rest], Atoms, Imports,
                FuncAcc, LabelAcc, CurrentFun, CurrentInstrs, PCOffset) ->
    %% Regular instruction - normalize and add
    Normalized = normalize_instruction(Instr, Atoms, Imports),
    NewInstrs = [Normalized | CurrentInstrs],  % Prepend to build in reverse
    split_functions(Rest, Atoms, Imports, FuncAcc, LabelAcc,
                    CurrentFun, NewInstrs, PCOffset + 1).

%% Normalize instruction format to match what execute_instr expects
normalize_instruction({call_ext, [Arity, Extfunc]}, _Atoms, Imports) ->
    %% call_ext with import reference — parser outputs {integer, Idx} for u-tagged values
    ImportIdx = case Extfunc of
        {integer, I} -> I;
        {atom, I} -> I;
        _ -> -1
    end,
    case ImportIdx >= 0 of
        true ->
            case nth_bare(ImportIdx + 1, Imports) of
                {ModIdx, FunIdx, ImportArity} ->
                    {call_ext, ImportArity, {extfunc_idx, ModIdx, FunIdx, ImportArity}};
                _ ->
                    {call_ext, Arity, {extfunc_idx, 0, 0, Arity}}
            end;
        false ->
            {call_ext, Arity, Extfunc}
    end;

normalize_instruction({call_ext_only, [Arity, Extfunc]}, _Atoms, Imports) ->
    ImportIdx = case Extfunc of
        {integer, I} -> I;
        {atom, I} -> I;
        _ -> -1
    end,
    case ImportIdx >= 0 of
        true ->
            case nth_bare(ImportIdx + 1, Imports) of
                {ModIdx, FunIdx, ImportArity} ->
                    {call_ext_only, ImportArity, {extfunc_idx, ModIdx, FunIdx, ImportArity}};
                _ ->
                    {call_ext_only, Arity, {extfunc_idx, 0, 0, Arity}}
            end;
        false ->
            {call_ext_only, Arity, Extfunc}
    end;

normalize_instruction({move, Operands}, _Atoms, _Imports) ->
    {move, Operands};

normalize_instruction({gc_bif, Operands}, _Atoms, _Imports) ->
    {gc_bif, Operands};

normalize_instruction({bif, Operands}, _Atoms, _Imports) ->
    {bif, Operands};

normalize_instruction({OpName, Operands}, _Atoms, _Imports) ->
    %% Generic normalization — convert {label,L} → {f,L} for consistent pattern matching
    NormOps = [normalize_operand(Op) || Op <- Operands],
    {OpName, NormOps}.

%% Convert parser operand tags to interpreter-canonical form
normalize_operand({label, L}) -> {f, L};
normalize_operand(Other) -> Other.

%% Get function code from module
get_function_code(Module, Function, Arity) ->
    Functions = maps:get(functions, Module, #{}),
    case maps:get({Function, Arity}, Functions, undefined) of
        undefined -> {error, {function_not_found, Function, Arity}};
        Code -> {ok, Code}
    end.

%% ============================================================================
%% Execution Engine
%% ============================================================================

%% Initialize execution state
init_state(Module, Function, Arity, Args, Instructions, OutputFun) ->
    %% Initialize X registers with arguments
    InitX = init_x_registers(Args, 0, #{}),

    #{
        module => Module,
        current_fun => {Function, Arity},
        current_instrs => Instructions,
        pc => 0,
        x => InitX,           % X registers: #{{x, N} => Value}
        y => [],              % Y registers (stack): [Value1, Value2, ...]
        stack => [],          % Call stack: [{Fun, Arity, PC}, ...]
        output_fun => OutputFun,
        reductions => 1000000  % Reduction budget
    }.

init_x_registers([], _Index, Acc) ->
    Acc;
init_x_registers([Arg | Rest], Index, Acc) ->
    init_x_registers(Rest, Index + 1, Acc#{{x, Index} => Arg}).

%% Main execution loop
execute(State) ->
    %% Check reduction budget
    Reds = maps:get(reductions, State),
    case Reds =< 0 of
        true ->
            {error, reduction_limit_exceeded};
        false ->
            %% Decrement reductions
            State1 = State#{reductions => Reds - 1},
            case execute_instruction(State1) of
                {continue, NewState} ->
                    execute(NewState);
                {return, Value} ->
                    {ok, Value};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Execute one instruction
execute_instruction(#{current_instrs := Instrs, pc := PC} = State) ->
    case nth_bare(PC + 1, Instrs) of
        undefined ->
            %% End of function
            handle_end_of_function(State);
        Instr ->
            execute_instr(Instr, State)
    end.

handle_end_of_function(#{stack := []} = State) ->
    %% Return from top-level
    RetVal = get_register({x, 0}, State),
    {return, RetVal};
handle_end_of_function(#{stack := [{RetFun, RetPC} | RestStack]} = State) ->
    %% Return from call
    Module = maps:get(module, State),
    case get_function_code(Module, element(1, RetFun), element(2, RetFun)) of
        {ok, RetInstrs} ->
            {continue, State#{
                current_fun => RetFun,
                current_instrs => RetInstrs,
                pc => RetPC,
                stack => RestStack
                %% NOTE: y registers preserved - deallocate will remove frame
            }};
        {error, _} ->
            {error, {return_function_not_found, RetFun}}
    end.

%% ============================================================================
%% Instruction Implementations
%% ============================================================================

%% Metadata instructions
execute_instr({label, _L}, State) ->
    {continue, advance_pc(State)};

execute_instr({func_info, _Module, _Function, _Arity}, State) ->
    {continue, advance_pc(State)};

execute_instr({line, _LineInfo}, State) ->
    {continue, advance_pc(State)};

execute_instr({int_code_end}, State) ->
    handle_end_of_function(State);

%% Return instruction
execute_instr(return, State) ->
    handle_end_of_function(State);

%% Move instruction
execute_instr({move, [Src, Dst]}, State) ->
    Value = get_value(Src, State),
    NewState = set_register(Dst, Value, State),
    {continue, advance_pc(NewState)};

%% Local calls (labels normalized to {f, L} by normalize_operand)
execute_instr({call, [_Arity, {f, Label}]}, State) ->
    handle_local_call(Label, State, false);

execute_instr({call_only, [_Arity, {f, Label}]}, State) ->
    handle_local_call(Label, State, true);

execute_instr({call_last, [_Arity, {f, Label}, Dealloc]}, State) ->
    %% Apply deallocation before tail call
    Y = maps:get(y, State),
    NewY = deallocate_stack(unwrap_int(Dealloc), Y),
    handle_local_call(Label, State#{y => NewY}, true);

%% External calls (BIFs)
execute_instr({call_ext, Arity, {extfunc_idx, ModIdx, FunIdx, _}}, State) ->
    handle_bif_call(ModIdx, FunIdx, Arity, State, false);

execute_instr({call_ext_only, Arity, {extfunc_idx, ModIdx, FunIdx, _}}, State) ->
    handle_bif_call(ModIdx, FunIdx, Arity, State, true);

%% Stack management (operands are tagged — unwrap to raw integers)
execute_instr({allocate, [StackNeed, _Live]}, State) ->
    Y = maps:get(y, State),
    NewY = allocate_stack(unwrap_int(StackNeed), Y),
    {continue, advance_pc(State#{y => NewY})};

execute_instr({allocate_zero, [StackNeed, _Live]}, State) ->
    Y = maps:get(y, State),
    NewY = allocate_stack(unwrap_int(StackNeed), Y),
    {continue, advance_pc(State#{y => NewY})};

execute_instr({deallocate, [N]}, State) ->
    Y = maps:get(y, State),
    NewY = deallocate_stack(unwrap_int(N), Y),
    {continue, advance_pc(State#{y => NewY})};

execute_instr({test_heap, [_Need, _Live]}, State) ->
    %% No-op - we use Erlang's heap
    {continue, advance_pc(State)};

%% List operations
execute_instr({put_list, [Head, Tail, Dst]}, State) ->
    H = get_value(Head, State),
    T = get_value(Tail, State),
    NewState = set_register(Dst, [H | T], State),
    {continue, advance_pc(NewState)};

execute_instr({get_list, [Src, Head, Tail]}, State) ->
    case get_value(Src, State) of
        [H | T] ->
            State2 = set_register(Head, H, State),
            State3 = set_register(Tail, T, State2),
            {continue, advance_pc(State3)};
        _ ->
            {error, {badmatch, not_a_list}}
    end;

execute_instr({get_hd, [Src, Dst]}, State) ->
    case get_value(Src, State) of
        [H | _] ->
            NewState = set_register(Dst, H, State),
            {continue, advance_pc(NewState)};
        _ ->
            {error, {badarg, not_a_list}}
    end;

execute_instr({get_tl, [Src, Dst]}, State) ->
    case get_value(Src, State) of
        [_ | T] ->
            NewState = set_register(Dst, T, State),
            {continue, advance_pc(NewState)};
        _ ->
            {error, {badarg, not_a_list}}
    end;

%% Tuple operations
execute_instr({put_tuple2, [Dst | Elements]}, State) ->
    Values = [get_value(E, State) || E <- Elements],
    Tuple = list_to_tuple(Values),
    NewState = set_register(Dst, Tuple, State),
    {continue, advance_pc(NewState)};

execute_instr({get_tuple_element, [Src, Index, Dst]}, State) ->
    case get_value(Src, State) of
        Tuple when is_tuple(Tuple) ->
            IndexVal = get_value(Index, State),
            case is_integer(IndexVal) andalso IndexVal >= 0 andalso IndexVal < tuple_size(Tuple) of
                true ->
                    Element = element(IndexVal + 1, Tuple),
                    NewState = set_register(Dst, Element, State),
                    {continue, advance_pc(NewState)};
                false ->
                    {error, {bad_element_index, IndexVal}}
            end;
        _ ->
            {error, {badarg, not_a_tuple}}
    end;

%% Type tests
execute_instr({is_integer, [{f, FailLabel}, Arg]}, State) ->
    Value = get_value(Arg, State),
    case is_integer(Value) of
        true -> {continue, advance_pc(State)};
        false -> jump_to_label(FailLabel, State)
    end;

execute_instr({is_atom, [{f, FailLabel}, Arg]}, State) ->
    Value = get_value(Arg, State),
    case is_atom(Value) of
        true -> {continue, advance_pc(State)};
        false -> jump_to_label(FailLabel, State)
    end;

execute_instr({is_list, [{f, FailLabel}, Arg]}, State) ->
    Value = get_value(Arg, State),
    case is_list(Value) of
        true -> {continue, advance_pc(State)};
        false -> jump_to_label(FailLabel, State)
    end;

execute_instr({is_nil, [{f, FailLabel}, Arg]}, State) ->
    Value = get_value(Arg, State),
    case Value of
        [] -> {continue, advance_pc(State)};
        _ -> jump_to_label(FailLabel, State)
    end;

%% Comparison tests
execute_instr({is_eq_exact, [{f, FailLabel}, Arg1, Arg2]}, State) ->
    V1 = get_value(Arg1, State),
    V2 = get_value(Arg2, State),
    case V1 =:= V2 of
        true -> {continue, advance_pc(State)};
        false -> jump_to_label(FailLabel, State)
    end;

execute_instr({is_ne_exact, [{f, FailLabel}, Arg1, Arg2]}, State) ->
    V1 = get_value(Arg1, State),
    V2 = get_value(Arg2, State),
    case V1 =/= V2 of
        true -> {continue, advance_pc(State)};
        false -> jump_to_label(FailLabel, State)
    end;

%% Jump
execute_instr({jump, [{f, Label}]}, State) ->
    jump_to_label(Label, State);

%% BIF calls
execute_instr({gc_bif, Operands}, State) when is_list(Operands) ->
    %% gc_bif format: [BifName, FailLabel, Live, Args, Dst]
    %% or sometimes: [BifName, FailLabel, Live, ArgsList, Dst]
    case Operands of
        [BifName, _FailLabel, _Live, Args, Dst] when is_list(Args) ->
            handle_gc_bif(BifName, Args, Dst, State);
        [BifName, _FailLabel, _Live, {list, Args}, Dst] ->
            handle_gc_bif(BifName, Args, Dst, State);
        [BifName, _FailLabel, _Live | Rest] ->
            %% Args not in list - take all remaining except last
            {Args, [Dst]} = split_at_last(Rest),
            handle_gc_bif(BifName, Args, Dst, State);
        _ ->
            {error, {bad_gc_bif_format, Operands}}
    end;

execute_instr({bif, Operands}, State) when is_list(Operands) ->
    case Operands of
        [BifName, _FailLabel, Args, Dst] when is_list(Args) ->
            handle_bif(BifName, Args, Dst, State);
        [BifName, _FailLabel, {list, Args}, Dst] ->
            handle_bif(BifName, Args, Dst, State);
        _ ->
            {error, {bad_bif_format, Operands}}
    end;

%% Unknown opcodes - return error instead of silently continuing
execute_instr({{unknown_opcode, N}, Args}, State) ->
    {error, {unknown_opcode, N, Args, maps:get(pc, State)}};

%% Unknown instruction - halt instead of silently continuing
execute_instr(Instr, State) ->
    {error, {unknown_instruction, Instr, maps:get(pc, State)}}.

%% ============================================================================
%% Call Handlers
%% ============================================================================

handle_local_call(Label, State, IsTailCall) ->
    Labels = maps:get(labels, maps:get(module, State)),
    case maps:get(Label, Labels, undefined) of
        undefined ->
            {error, {label_not_found, Label}};
        {TargetFun, _InstrIndex} ->
            Module = maps:get(module, State),
            case get_function_code(Module, element(1, TargetFun), element(2, TargetFun)) of
                {ok, TargetInstrs} ->
                    NewStack = case IsTailCall of
                        true -> maps:get(stack, State);
                        false ->
                            CurrentFun = maps:get(current_fun, State),
                            PC = maps:get(pc, State),
                            [{CurrentFun, PC + 1} | maps:get(stack, State)]
                    end,
                    %% FIXED: Don't clear Y stack on tail call - frame already deallocated if needed
                    NewState = State#{
                        current_fun => TargetFun,
                        current_instrs => TargetInstrs,
                        pc => 0,
                        stack => NewStack
                    },
                    {continue, NewState};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

handle_bif_call(ModIdx, FunIdx, Arity, State, IsTailCall) ->
    %% Validate arity is within bounds
    case is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255 of
        false ->
            {error, {invalid_arity, Arity}};
        true ->
            %% Resolve atom indices to actual names (may be binaries from parser)
            Module = maps:get(module, State),
            Atoms = maps:get(atoms, Module),
            ModRaw = nth_bare(ModIdx, Atoms),
            FunRaw = nth_bare(FunIdx, Atoms),
            %% Normalize binaries to atoms for BIF dispatch
            ModAtom = safe_to_atom(ModRaw),
            FunAtom = safe_to_atom(FunRaw),

            %% Get arguments from X registers
            Args = [get_register({x, I}, State) || I <- seq_bare(0, Arity - 1)],
            handle_bif_call_inner(ModAtom, FunAtom, Args, Arity, State, IsTailCall)
    end.

handle_bif_call_inner(ModAtom, FunAtom, Args, Arity, State, IsTailCall) ->

    %% Execute BIF
    case execute_bif(ModAtom, FunAtom, Args, State) of
        {ok, Result} ->
            case IsTailCall of
                true ->
                    %% Put result in x0 and let return flow through normal path
                    NewState = set_register({x, 0}, Result, State),
                    execute_instr(return, NewState);
                false ->
                    NewState = set_register({x, 0}, Result, State),
                    {continue, advance_pc(NewState)}
            end;
        {error, Reason} ->
            {error, {bif_error, ModAtom, FunAtom, Arity, Reason}}
    end.

handle_gc_bif(BifName, Args, Dst, State) ->
    BifAtom = case BifName of
        {atom, A} -> safe_to_atom(A);
        _ -> safe_to_atom(BifName)
    end,
    ArgValues = [get_value(A, State) || A <- Args],
    case execute_bif(erlang, BifAtom, ArgValues, State) of
        {ok, Result} ->
            NewState = set_register(Dst, Result, State),
            {continue, advance_pc(NewState)};
        {error, Reason} ->
            {error, {bif_error, erlang, BifAtom, length_bare(Args), Reason}}
    end.

handle_bif(BifName, Args, Dst, State) ->
    handle_gc_bif(BifName, Args, Dst, State).

%% ============================================================================
%% Built-in Functions (BIFs)
%% ============================================================================

%% Arithmetic BIFs
execute_bif(erlang, '+', [A, B], _State) when is_number(A), is_number(B) ->
    {ok, A + B};

execute_bif(erlang, '-', [A, B], _State) when is_number(A), is_number(B) ->
    {ok, A - B};

execute_bif(erlang, '*', [A, B], _State) when is_number(A), is_number(B) ->
    {ok, A * B};

execute_bif(erlang, 'div', [A, B], _State) when is_integer(A), is_integer(B), B =/= 0 ->
    {ok, A div B};

execute_bif(erlang, 'rem', [A, B], _State) when is_integer(A), is_integer(B), B =/= 0 ->
    {ok, A rem B};

execute_bif(erlang, '/', [A, B], _State) when is_number(A), is_number(B), B =/= 0 ->
    {ok, A / B};

%% Comparison BIFs
execute_bif(erlang, '==', [A, B], _State) ->
    {ok, A == B};

execute_bif(erlang, '/=', [A, B], _State) ->
    {ok, A /= B};

execute_bif(erlang, '<', [A, B], _State) ->
    {ok, A < B};

execute_bif(erlang, '>', [A, B], _State) ->
    {ok, A > B};

execute_bif(erlang, '=<', [A, B], _State) ->
    {ok, A =< B};

execute_bif(erlang, '>=', [A, B], _State) ->
    {ok, A >= B};

%% Boolean BIFs
execute_bif(erlang, 'not', [A], _State) ->
    {ok, not A};

%% List BIFs
execute_bif(erlang, length, [List], _State) when is_list(List) ->
    {ok, length_bare(List)};

execute_bif(lists, reverse, [List], _State) when is_list(List) ->
    {ok, reverse_bare(List)};

%% Output BIFs - use callback
execute_bif(io, format, [Format], State) when is_list(Format) ->
    OutputFun = maps:get(output_fun, State),
    format_bare(Format, [], OutputFun),
    {ok, ok};

execute_bif(io, format, [Format], State) when is_binary(Format) ->
    OutputFun = maps:get(output_fun, State),
    format_bare(binary_to_list(Format), [], OutputFun),
    {ok, ok};

execute_bif(io, format, [Format, Args], State) when is_list(Format), is_list(Args) ->
    %% Bare-metal format: handle ~s, ~p, ~w, ~n substitutions
    OutputFun = maps:get(output_fun, State),
    format_bare(Format, Args, OutputFun),
    {ok, ok};

execute_bif(erlang, display, [Arg], State) ->
    OutputFun = maps:get(output_fun, State),
    %% Convert to string manually (no io_lib:format)
    Str = term_to_string(Arg),
    OutputFun(Str),
    OutputFun("\n"),
    {ok, ok};

%% Fallback for unknown BIFs
execute_bif(Mod, Fun, Args, State) ->
    OutputFun = maps:get(output_fun, State),
    %% Mod and Fun are now binaries (from parser atom table)
    ModStr = if is_binary(Mod) -> binary_to_list(Mod);
                is_atom(Mod) -> atom_to_list(Mod);
                true -> "unknown"
             end,
    FunStr = if is_binary(Fun) -> binary_to_list(Fun);
                is_atom(Fun) -> atom_to_list(Fun);
                true -> "unknown"
             end,
    Msg = "Unknown BIF: " ++ ModStr ++ ":" ++
          FunStr ++ "/" ++ integer_to_list(length_bare(Args)) ++ "\n",
    OutputFun(Msg),
    {error, {undef, Mod, Fun, length_bare(Args)}}.

%% Bare-metal io:format — handles ~s, ~p, ~w, ~n, ~c
format_bare([], _Args, _Out) -> ok;
format_bare([$~, $n | Rest], Args, Out) ->
    Out("\n"),
    format_bare(Rest, Args, Out);
format_bare([$~, $s | Rest], [Arg | Args], Out) ->
    %% ~s: output string/binary as-is
    case Arg of
        B when is_binary(B) -> Out(binary_to_list(B));
        L when is_list(L) -> Out(L);
        _ -> Out(term_to_string(Arg))
    end,
    format_bare(Rest, Args, Out);
format_bare([$~, $p | Rest], [Arg | Args], Out) ->
    Out(term_to_string(Arg)),
    format_bare(Rest, Args, Out);
format_bare([$~, $w | Rest], [Arg | Args], Out) ->
    Out(term_to_string(Arg)),
    format_bare(Rest, Args, Out);
format_bare([$~, $c | Rest], [Arg | Args], Out) ->
    Out([Arg]),
    format_bare(Rest, Args, Out);
format_bare([$~ | Rest], Args, Out) ->
    %% Unknown format spec — skip it
    case Rest of
        [_ | Rest2] ->
            case Args of
                [_ | ArgsRest] -> format_bare(Rest2, ArgsRest, Out);
                [] -> format_bare(Rest2, [], Out)
            end;
        [] -> ok
    end;
format_bare([C | Rest], Args, Out) ->
    Out([C]),
    format_bare(Rest, Args, Out).

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Advance program counter
advance_pc(State) ->
    PC = maps:get(pc, State),
    State#{pc => PC + 1}.

%% Jump to label
jump_to_label(Label, State) ->
    Labels = maps:get(labels, maps:get(module, State)),
    case maps:get(Label, Labels, undefined) of
        undefined ->
            {error, {label_not_found, Label}};
        {Fun, InstrIndex} ->
            CurrentFun = maps:get(current_fun, State),
            case Fun =:= CurrentFun of
                true ->
                    %% Jump within current function
                    {continue, State#{pc => InstrIndex}};
                false ->
                    %% Jump to different function
                    Module = maps:get(module, State),
                    case get_function_code(Module, element(1, Fun), element(2, Fun)) of
                        {ok, Instrs} ->
                            {continue, State#{
                                current_fun => Fun,
                                current_instrs => Instrs,
                                pc => InstrIndex
                            }};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% Unwrap tagged integer to raw value
unwrap_int({integer, N}) -> N;
unwrap_int(N) when is_integer(N) -> N;
unwrap_int(_) -> 0.

%% Get value from source operand
get_value({x, N}, State) ->
    get_register({x, N}, State);
get_value({y, N}, State) ->
    get_register({y, N}, State);
get_value({atom, Atom}, _State) ->
    Atom;
get_value({integer, Int}, _State) ->
    Int;
get_value({literal, Idx}, State) ->
    Module = maps:get(module, State),
    Literals = maps:get(literals, Module),
    nth_bare(Idx + 1, Literals);
get_value(nil, _State) ->
    [];
get_value({label, L}, _State) ->
    {f, L};
get_value(Value, _State) ->
    Value.

%% Get register value
get_register({x, N}, State) ->
    X = maps:get(x, State),
    maps:get({x, N}, X, undefined);
get_register({y, N}, State) ->
    Y = maps:get(y, State),
    nth_bare(N + 1, Y);
get_register(_Other, _State) ->
    undefined.

%% Set register value
set_register({x, N}, Value, State) ->
    X = maps:get(x, State),
    State#{x => X#{{x, N} => Value}};
set_register({y, N}, Value, State) ->
    %% SECURITY FIX (Finding #6): Validate Y index bounds before write
    case is_integer(N) andalso N >= 0 andalso N =< 1024 of
        true ->
            Y = maps:get(y, State),
            NewY = set_nth_bare(N + 1, Value, Y),
            State#{y => NewY};
        false ->
            %% Invalid Y index - return state unchanged
            State
    end;
set_register(_Other, _Value, State) ->
    State.

%% Stack management
allocate_stack(N, Y) ->
    %% Validate N is within bounds
    case is_integer(N) andalso N >= 0 andalso N =< 1024 of
        true ->
            duplicate_bare(N, undefined) ++ Y;
        false ->
            Y  % Return unchanged on error
    end.

deallocate_stack(N, Y) ->
    nthtail_bare(N, Y).

%% Convert term to string (minimal implementation)
term_to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
term_to_string(Int) when is_integer(Int) ->
    integer_to_list(Int);
term_to_string(List) when is_list(List) ->
    %% Check if it's a string
    case is_string_bare(List) of
        true -> List;
        false -> "[...]"  % Simplified - full list formatting is complex
    end;
term_to_string(Tuple) when is_tuple(Tuple) ->
    "{...}";  % Simplified
term_to_string(_Other) ->
    "<<term>>".

%% ============================================================================
%% Self-contained List Operations (no lists:* imports)
%% ============================================================================

length_bare(List) ->
    length_bare_acc(List, 0).

length_bare_acc([], Acc) ->
    Acc;
length_bare_acc([_ | T], Acc) ->
    length_bare_acc(T, Acc + 1).

reverse_bare(List) ->
    reverse_bare_acc(List, []).

reverse_bare_acc([], Acc) ->
    Acc;
reverse_bare_acc([H | T], Acc) ->
    reverse_bare_acc(T, [H | Acc]).

%% Convert binary or atom to atom for BIF dispatch
%% If atom doesn't exist, return the binary itself
safe_to_atom(undefined) -> undefined;  % CRITICAL FIX: Handle undefined from invalid import index
safe_to_atom(A) when is_atom(A) -> A;
safe_to_atom(B) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg ->
        try binary_to_existing_atom(B, latin1)
        catch error:badarg -> B  % Return binary as-is, don't create atom
        end
    end.

nth_bare(_N, []) ->
    undefined;
nth_bare(1, [H | _]) ->
    H;
nth_bare(N, [_ | T]) when N > 1 ->
    nth_bare(N - 1, T);
nth_bare(_, _) ->
    undefined.

set_nth_bare(1, Value, [_ | T]) ->
    [Value | T];
set_nth_bare(N, Value, [H | T]) when N > 1 ->
    [H | set_nth_bare(N - 1, Value, T)];
set_nth_bare(N, Value, []) when N > 0 ->
    %% Extend list if needed
    duplicate_bare(N - 1, undefined) ++ [Value];
set_nth_bare(_, _, List) ->
    List.

duplicate_bare(0, _) ->
    [];
duplicate_bare(N, Item) when N > 0 ->
    [Item | duplicate_bare(N - 1, Item)].

nthtail_bare(0, List) ->
    List;
nthtail_bare(N, [_ | T]) when N > 0 ->
    nthtail_bare(N - 1, T);
nthtail_bare(_, _) ->
    [].

seq_bare(From, To) when From > To ->
    [];
seq_bare(From, To) ->
    [From | seq_bare(From + 1, To)].

is_string_bare([]) ->
    true;
is_string_bare([H | T]) when is_integer(H), H >= 0, H =< 255 ->
    is_string_bare(T);
is_string_bare(_) ->
    false.

split_at_last([]) ->
    {[], []};
split_at_last([X]) ->
    {[], [X]};
split_at_last([H | T]) ->
    {Rest, Last} = split_at_last(T),
    {[H | Rest], Last}.
