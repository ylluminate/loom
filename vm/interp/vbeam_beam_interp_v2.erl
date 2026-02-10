%% BEAM Bytecode Interpreter v2
%% Uses beam_disasm for instruction decoding
%% Implements minimal opcode set for V-compiled programs

-module(vbeam_beam_interp_v2).
-export([execute_file/2, execute_file/3, execute/3, execute/4]).

-record(proc, {
    module,         % Current module atom
    code_map,       % Function -> Instructions mapping
    labels,         % Label -> {Function, InstrIndex} mapping
    x = #{},        % X registers (x0-x1023)
    y = [],         % Y registers (stack frame)
    pc = 0,         % Program counter (instruction index within function)
    cp = undefined, % Continuation pointer (return address)
    stack = [],     % Call stack: [{Module, Function, PC}]
    current_fun,    % Current function {Name, Arity}
    current_instrs, % Current function's instruction list
    reductions = 1000000  % Reduction budget
}).

%% Execute a function from a .beam file
execute_file(Path, FunctionName) ->
    execute_file(Path, FunctionName, []).

execute_file(Path, FunctionName, Args) ->
    case beam_disasm:file(Path) of
        {beam_file, Module, _Exports, _Attrs, _CompileInfo, Code} ->
            execute({Module, Code}, FunctionName, Args);
        {error, Reason} ->
            {error, {beam_disasm, Reason}}
    end.

%% Execute a function from disassembled code
execute({Module, Code}, FunctionName, Args) ->
    execute({Module, Code}, FunctionName, Args, []).

execute({Module, Code}, FunctionName, Args, Options) when is_list(Args) ->
    %% Build code map and label map
    CodeMap = build_code_map(Code),
    Labels = build_label_map(Code),

    %% Safely convert function name to atom - only if it already exists
    FunAtom = case FunctionName of
        A when is_atom(A) -> A;
        S when is_list(S) ->
            try
                list_to_existing_atom(S)
            catch
                error:badarg ->
                    %% Atom doesn't exist - function not found
                    undefined
            end;
        _ -> undefined
    end,

    Arity = length(Args),

    case FunAtom of
        undefined ->
            {error, {function_not_found, FunctionName, Arity}};
        _ ->
            case maps:get({FunAtom, Arity}, CodeMap, undefined) of
                undefined ->
                    {error, {function_not_found, FunAtom, Arity}};
        Instructions ->
            %% Initialize process state
            InitX = lists:foldl(
                fun({Arg, Index}, Acc) ->
                    Acc#{{x, Index} => Arg}
                end,
                #{},
                lists:zip(Args, lists:seq(0, Arity - 1))
            ),

            Proc = #proc{
                module = Module,
                code_map = CodeMap,
                labels = Labels,
                x = InitX,
                current_fun = {FunAtom, Arity},
                current_instrs = Instructions,
                pc = 0
            },

            %% Run the interpreter
            run(Proc, Options)
            end
    end;
execute(_Module, _FunctionName, Args, _Options) ->
    {error, {invalid_args, Args}}.

%% Build function -> instructions mapping
build_code_map(Code) ->
    lists:foldl(
        fun({function, Name, Arity, _Entry, Instructions}, Acc) ->
            Acc#{{Name, Arity} => Instructions}
        end,
        #{},
        Code
    ).

%% Build label -> {Function, InstrIndex} mapping
build_label_map(Code) ->
    lists:foldl(
        fun({function, Name, Arity, _Entry, Instructions}, Acc) ->
            lists:foldl(
                fun({Index, Instr}, AccInner) ->
                    case Instr of
                        {label, L} ->
                            AccInner#{L => {{Name, Arity}, Index}};
                        _ ->
                            AccInner
                    end
                end,
                Acc,
                lists:zip(lists:seq(0, length(Instructions) - 1), Instructions)
            )
        end,
        #{},
        Code
    ).

%% Main interpreter loop
run(Proc, Options) ->
    case execute_instruction(Proc, Options) of
        {continue, Proc2} ->
            run(Proc2, Options);
        {return, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% Execute one instruction
execute_instruction(#proc{reductions = Reds} = Proc, Options) ->
    %% Check reduction budget
    case Reds =< 0 of
        true ->
            {error, reduction_limit_exceeded};
        false ->
            %% Decrement reductions
            Proc1 = Proc#proc{reductions = Reds - 1},
            execute_instruction_inner(Proc1, Options)
    end.

execute_instruction_inner(#proc{current_instrs = Instrs, pc = PC} = Proc, Options) ->
    case PC >= length(Instrs) of
        true ->
            %% End of function - return
            case Proc#proc.stack of
                [] ->
                    RetVal = get_register({x, 0}, Proc),
                    {return, RetVal};
                [{_RetModule, RetFun, RetPC} | RestStack] ->
                    RetInstrs = maps:get(RetFun, Proc#proc.code_map),
                    {continue, Proc#proc{
                        current_fun = RetFun,
                        current_instrs = RetInstrs,
                        pc = RetPC,
                        stack = RestStack
                    }}
            end;
        false ->
            Instr = lists:nth(PC + 1, Instrs),
            execute_instr(Instr, Proc, Options)
    end.

%% Execute specific instructions
execute_instr({label, _L}, Proc, _Options) ->
    %% Label - just advance PC
    {continue, Proc#proc{pc = Proc#proc.pc + 1}};

execute_instr({func_info, _Module, _Function, _Arity}, Proc, _Options) ->
    %% Function info - advance PC (used for error reporting)
    {continue, Proc#proc{pc = Proc#proc.pc + 1}};

execute_instr(return, Proc, _Options) ->
    %% Return from function
    case Proc#proc.stack of
        [] ->
            %% Return from top-level
            RetVal = get_register({x, 0}, Proc),
            {return, RetVal};
        [{_RetModule, RetFun, RetPC} | RestStack] ->
            RetInstrs = maps:get(RetFun, Proc#proc.code_map),
            {continue, Proc#proc{
                current_fun = RetFun,
                current_instrs = RetInstrs,
                pc = RetPC,
                stack = RestStack
                %% NOTE: y registers preserved - deallocate will remove frame
            }}
    end;

execute_instr({move, Src, Dst}, Proc, _Options) ->
    %% Move value from Src to Dst
    Value = get_value(Src, Proc),
    Proc2 = set_register(Dst, Value, Proc),
    {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};

execute_instr({call, _Arity, {f, Label}}, Proc, _Options) ->
    %% Call local function
    case maps:get(Label, Proc#proc.labels, undefined) of
        undefined ->
            {error, {label_not_found, Label}};
        {TargetFun, InstrIndex} ->
            TargetInstrs = maps:get(TargetFun, Proc#proc.code_map),
            %% Push return address
            NewStack = [{Proc#proc.module, Proc#proc.current_fun, Proc#proc.pc + 1} | Proc#proc.stack],
            %% CRITICAL FIX (Round 42, Finding 4): Use InstrIndex from label, not 0.
            %% Labels can point to non-zero instruction offsets within a function.
            {continue, Proc#proc{
                current_fun = TargetFun,
                current_instrs = TargetInstrs,
                pc = InstrIndex,
                stack = NewStack
            }}
    end;

execute_instr({call, Arity, {Module, Function, Arity}}, Proc, _Options) ->
    %% Call local function via MFA tuple (beam_disasm format)
    TargetFun = {Function, Arity},
    case maps:get(TargetFun, Proc#proc.code_map, undefined) of
        undefined ->
            {error, {function_not_found, Module, Function, Arity}};
        TargetInstrs ->
            %% Push return address
            NewStack = [{Proc#proc.module, Proc#proc.current_fun, Proc#proc.pc + 1} | Proc#proc.stack],
            {continue, Proc#proc{
                current_fun = TargetFun,
                current_instrs = TargetInstrs,
                pc = 0,
                stack = NewStack
            }}
    end;

execute_instr({call_only, _Arity, {f, Label}}, Proc, _Options) ->
    %% Tail call - don't push return address
    %% NOTE: Don't clear Y stack - current frame (if any) was already deallocated
    case maps:get(Label, Proc#proc.labels, undefined) of
        undefined ->
            {error, {label_not_found, Label}};
        {TargetFun, InstrIndex} ->
            TargetInstrs = maps:get(TargetFun, Proc#proc.code_map),
            %% CRITICAL FIX (Round 42, Finding 4): Use InstrIndex from label.
            {continue, Proc#proc{
                current_fun = TargetFun,
                current_instrs = TargetInstrs,
                pc = InstrIndex
            }}
    end;

execute_instr({call_only, Arity, {Module, Function, Arity}}, Proc, _Options) ->
    %% Tail call via MFA tuple (beam_disasm format)
    TargetFun = {Function, Arity},
    case maps:get(TargetFun, Proc#proc.code_map, undefined) of
        undefined ->
            {error, {function_not_found, Module, Function, Arity}};
        TargetInstrs ->
            {continue, Proc#proc{
                current_fun = TargetFun,
                current_instrs = TargetInstrs,
                pc = 0
            }}
    end;

execute_instr({call_ext, Arity, {extfunc, Mod, Fun, Arity}}, Proc, Options) ->
    %% FINDING 10 FIX: Validate arity before building argument list
    case is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255 of
        true ->
            %% Call external function (BIF)
            Args = [get_register({x, I}, Proc) || I <- lists:seq(0, Arity - 1)],
            case execute_bif(Mod, Fun, Args, Options) of
                {ok, Result} ->
                    Proc2 = set_register({x, 0}, Result, Proc),
                    {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
                {error, Reason} ->
                    {error, {bif_error, Mod, Fun, Arity, Reason}}
            end;
        false ->
            {error, {invalid_arity, Arity}}
    end;

execute_instr({call_ext_only, Arity, {extfunc, Mod, Fun, Arity}}, Proc, Options) ->
    %% FINDING 10 FIX: Validate arity before building argument list
    case is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255 of
        true ->
            %% Tail call to external function
            Args = [get_register({x, I}, Proc) || I <- lists:seq(0, Arity - 1)],
            case execute_bif(Mod, Fun, Args, Options) of
                {ok, Result} ->
                    %% Put result in x0 and let return instruction handle stack unwinding
                    Proc2 = set_register({x, 0}, Result, Proc),
                    execute_instr(return, Proc2, Options);
                {error, Reason} ->
                    {error, {bif_error, Mod, Fun, Arity, Reason}}
            end;
        false ->
            {error, {invalid_arity, Arity}}
    end;

execute_instr({allocate, StackNeed, _Live}, Proc, _Options) ->
    %% Allocate stack frame - validate StackNeed
    case is_integer(StackNeed) andalso StackNeed >= 0 andalso StackNeed =< 1024 of
        true ->
            NewY = lists:duplicate(StackNeed, undefined) ++ Proc#proc.y,
            {continue, Proc#proc{y = NewY, pc = Proc#proc.pc + 1}};
        false ->
            {error, {invalid_allocate, StackNeed}}
    end;

execute_instr({allocate_zero, StackNeed, _Live}, Proc, _Options) ->
    %% Allocate and zero stack frame - validate StackNeed
    case is_integer(StackNeed) andalso StackNeed >= 0 andalso StackNeed =< 1024 of
        true ->
            NewY = lists:duplicate(StackNeed, undefined) ++ Proc#proc.y,
            {continue, Proc#proc{y = NewY, pc = Proc#proc.pc + 1}};
        false ->
            {error, {invalid_allocate_zero, StackNeed}}
    end;

execute_instr({deallocate, N}, Proc, _Options) ->
    %% Deallocate stack frame
    Y = Proc#proc.y,
    case is_integer(N) andalso N >= 0 andalso N =< length(Y) of
        true ->
            NewY = lists:nthtail(N, Y),
            {continue, Proc#proc{y = NewY, pc = Proc#proc.pc + 1}};
        false ->
            {error, {invalid_deallocate, N}}
    end;

execute_instr({test_heap, _Need, _Live}, Proc, _Options) ->
    %% Heap test - no-op (we use Erlang's heap)
    {continue, Proc#proc{pc = Proc#proc.pc + 1}};

execute_instr({put_list, Head, Tail, Dst}, Proc, _Options) ->
    %% Construct [Head | Tail]
    H = get_value(Head, Proc),
    T = get_value(Tail, Proc),
    Proc2 = set_register(Dst, [H | T], Proc),
    {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};

execute_instr({get_list, Src, Head, Tail}, Proc, _Options) ->
    %% Destructure [Head | Tail]
    case get_value(Src, Proc) of
        [H | T] ->
            Proc2 = set_register(Head, H, Proc),
            Proc3 = set_register(Tail, T, Proc2),
            {continue, Proc3#proc{pc = Proc3#proc.pc + 1}};
        _ ->
            {error, {badmatch, not_a_list}}
    end;

execute_instr({get_hd, Src, Dst}, Proc, _Options) ->
    %% Get list head
    case get_value(Src, Proc) of
        [H | _] ->
            Proc2 = set_register(Dst, H, Proc),
            {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
        _ ->
            {error, {badarg, not_a_list}}
    end;

execute_instr({get_tl, Src, Dst}, Proc, _Options) ->
    %% Get list tail
    case get_value(Src, Proc) of
        [_ | T] ->
            Proc2 = set_register(Dst, T, Proc),
            {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
        _ ->
            {error, {badarg, not_a_list}}
    end;

execute_instr({put_tuple2, Dst, Elements}, Proc, _Options) ->
    %% Construct tuple
    Values = [get_value(E, Proc) || E <- Elements],
    Tuple = list_to_tuple(Values),
    Proc2 = set_register(Dst, Tuple, Proc),
    {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};

execute_instr({get_tuple_element, Src, Index, Dst}, Proc, _Options) ->
    %% Extract tuple element (0-indexed)
    case get_value(Src, Proc) of
        Tuple when is_tuple(Tuple) ->
            case is_integer(Index) andalso Index >= 0 andalso Index < tuple_size(Tuple) of
                true ->
                    Element = element(Index + 1, Tuple),
                    Proc2 = set_register(Dst, Element, Proc),
                    {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
                false ->
                    {error, {bad_element_index, Index}}
            end;
        _ ->
            {error, {badarg, not_a_tuple}}
    end;

execute_instr({is_eq_exact, {f, FailLabel}, Arg1, Arg2}, Proc, _Options) ->
    %% Test exact equality
    V1 = get_value(Arg1, Proc),
    V2 = get_value(Arg2, Proc),
    case V1 =:= V2 of
        true ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        false ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({is_ne_exact, {f, FailLabel}, Arg1, Arg2}, Proc, _Options) ->
    %% Test exact inequality
    V1 = get_value(Arg1, Proc),
    V2 = get_value(Arg2, Proc),
    case V1 =/= V2 of
        true ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        false ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({is_integer, {f, FailLabel}, Arg}, Proc, _Options) ->
    %% Test if integer
    Value = get_value(Arg, Proc),
    case is_integer(Value) of
        true ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        false ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({is_atom, {f, FailLabel}, Arg}, Proc, _Options) ->
    %% Test if atom
    Value = get_value(Arg, Proc),
    case is_atom(Value) of
        true ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        false ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({is_list, {f, FailLabel}, Arg}, Proc, _Options) ->
    %% Test if list
    Value = get_value(Arg, Proc),
    case is_list(Value) of
        true ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        false ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({is_nil, {f, FailLabel}, Arg}, Proc, _Options) ->
    %% Test if empty list
    Value = get_value(Arg, Proc),
    case Value of
        [] ->
            {continue, Proc#proc{pc = Proc#proc.pc + 1}};
        _ ->
            jump_to_label(FailLabel, Proc)
    end;

execute_instr({jump, {f, Label}}, Proc, _Options) ->
    %% Unconditional jump
    jump_to_label(Label, Proc);

execute_instr({gc_bif, BifName, {f, _FailLabel}, _Live, Args, Dst}, Proc, Options) ->
    %% BIF call with garbage collection
    ArgValues = [get_value(A, Proc) || A <- Args],
    case execute_bif(erlang, BifName, ArgValues, Options) of
        {ok, Result} ->
            Proc2 = set_register(Dst, Result, Proc),
            {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
        {error, Reason} ->
            {error, {bif_error, erlang, BifName, length(Args), Reason}}
    end;

execute_instr({bif, BifName, {f, _FailLabel}, Args, Dst}, Proc, Options) ->
    %% BIF call without garbage collection
    ArgValues = [get_value(A, Proc) || A <- Args],
    case execute_bif(erlang, BifName, ArgValues, Options) of
        {ok, Result} ->
            Proc2 = set_register(Dst, Result, Proc),
            {continue, Proc2#proc{pc = Proc2#proc.pc + 1}};
        {error, Reason} ->
            {error, {bif_error, erlang, BifName, length(Args), Reason}}
    end;

execute_instr({line, _LineInfo}, Proc, _Options) ->
    %% Line number info - just advance PC
    {continue, Proc#proc{pc = Proc#proc.pc + 1}};

execute_instr(Instr, Proc, _Options) ->
    %% Unknown instruction - halt instead of continuing
    {error, {unknown_instruction, Instr, Proc#proc.pc, Proc#proc.current_fun}}.

%% Jump to label
jump_to_label(Label, Proc) ->
    case maps:get(Label, Proc#proc.labels, undefined) of
        undefined ->
            {error, {label_not_found, Label}};
        {Fun, InstrIndex} when Fun =:= Proc#proc.current_fun ->
            %% Jump within current function
            {continue, Proc#proc{pc = InstrIndex}};
        {Fun, InstrIndex} ->
            %% Jump to different function
            Instrs = maps:get(Fun, Proc#proc.code_map),
            {continue, Proc#proc{
                current_fun = Fun,
                current_instrs = Instrs,
                pc = InstrIndex
            }}
    end.

%% Get value from source
get_value({x, N}, Proc) ->
    get_register({x, N}, Proc);
get_value({y, N}, Proc) ->
    get_register({y, N}, Proc);
get_value({atom, Atom}, _Proc) ->
    Atom;
get_value({integer, Int}, _Proc) ->
    Int;
get_value({literal, Lit}, _Proc) ->
    Lit;
get_value(nil, _Proc) ->
    [];
get_value(Value, _Proc) ->
    Value.

%% Get register value
get_register({x, N}, #proc{x = X}) ->
    maps:get({x, N}, X, undefined);
get_register({y, N}, #proc{y = Y}) ->
    case is_integer(N) andalso N >= 0 andalso N < length(Y) of
        true -> lists:nth(N + 1, Y);
        false -> undefined
    end;
get_register(_Other, _Proc) ->
    undefined.

%% Set register value
set_register({x, N}, Value, #proc{x = X} = Proc) ->
    Proc#proc{x = X#{{x, N} => Value}};
set_register({y, N}, Value, #proc{y = Y} = Proc) ->
    %% SECURITY FIX: Validate N is non-negative integer before using sublist/nthtail
    %% Also check bounds to prevent unbounded stack growth
    case is_integer(N) andalso N >= 0 of
        true ->
            Len = length(Y),
            %% Prevent excessive stack growth
            case N > Len + 1024 of
                true ->
                    Proc;  % Invalid - too far beyond current stack
                false ->
                    NewY = if
                        N < Len ->
                            lists:sublist(Y, N) ++ [Value] ++ lists:nthtail(N + 1, Y);
                        N =:= Len ->
                            Y ++ [Value];
                        true ->
                            Y ++ lists:duplicate(N - Len, undefined) ++ [Value]
                    end,
                    Proc#proc{y = NewY}
            end;
        false ->
            %% Invalid Y register index - return unchanged
            Proc
    end;
set_register(_Other, _Value, Proc) ->
    Proc.

%% Execute built-in function
execute_bif(erlang, display, [Arg], Options) ->
    case proplists:get_value(quiet, Options, false) of
        false -> io:format("~p~n", [Arg]);
        true -> ok
    end,
    {ok, ok};

execute_bif(erlang, '+', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A + B};

execute_bif(erlang, '-', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A - B};

execute_bif(erlang, '*', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A * B};

execute_bif(erlang, 'div', [A, B], _Options) when is_integer(A), is_integer(B), B =/= 0 ->
    {ok, A div B};

execute_bif(erlang, 'rem', [A, B], _Options) when is_integer(A), is_integer(B), B =/= 0 ->
    {ok, A rem B};

execute_bif(erlang, '/', [A, B], _Options) when is_number(A), is_number(B), B =/= 0 ->
    {ok, A / B};

execute_bif(erlang, '==', [A, B], _Options) ->
    {ok, A == B};

execute_bif(erlang, '/=', [A, B], _Options) ->
    {ok, A /= B};

execute_bif(erlang, '<', [A, B], _Options) ->
    {ok, A < B};

execute_bif(erlang, '>', [A, B], _Options) ->
    {ok, A > B};

execute_bif(erlang, '=<', [A, B], _Options) ->
    {ok, A =< B};

execute_bif(erlang, '>=', [A, B], _Options) ->
    {ok, A >= B};

execute_bif(erlang, 'not', [A], _Options) ->
    {ok, not A};

execute_bif(io, format, [Format], _Options) ->
    try
        io:format("~s", [Format]),
        {ok, ok}
    catch
        error:Reason ->
            {error, {format_error, Reason}}
    end;

execute_bif(io, format, [Format, Args], _Options) ->
    try
        io:format(Format, Args),
        {ok, ok}
    catch
        error:Reason ->
            {error, {format_error, Reason}}
    end;

execute_bif(lists, reverse, [List], _Options) when is_list(List) ->
    {ok, lists:reverse(List)};

%% FINDING 7 FIX: Add argument caps for lists:seq/2 and lists:duplicate/2
execute_bif(lists, seq, [From, To], _Options) when is_integer(From), is_integer(To) ->
    Range = abs(To - From) + 1,
    case Range =< 10000 of
        true ->
            {ok, lists:seq(From, To)};
        false ->
            {error, {argument_too_large, seq, Range, max, 10000}}
    end;

execute_bif(lists, duplicate, [N, _Elem], _Options) when is_integer(N), N > 10000 ->
    {error, {argument_too_large, duplicate, N, max, 10000}};

execute_bif(lists, duplicate, [N, Elem], _Options) when is_integer(N), N >= 0 ->
    {ok, lists:duplicate(N, Elem)};

execute_bif(Mod, Fun, Args, _Options) ->
    %% SECURITY: Enforce strict BIF allowlist — guest code must not execute arbitrary host functions
    %% Only allow specific safe modules and functions
    case is_allowed_bif(Mod, Fun, length(Args)) of
        true ->
            case catch apply(Mod, Fun, Args) of
                {'EXIT', Reason} ->
                    io:format("Warning: BIF ~p:~p/~p failed: ~p~n",
                              [Mod, Fun, length(Args), Reason]),
                    {error, Reason};
                Result ->
                    {ok, Result}
            end;
        capped ->
            %% This should not be reached (capped BIFs handled explicitly above)
            {error, {implementation_error, capped_bif_not_handled}};
        false ->
            {error, {undef, Mod, Fun, length(Args)}}
    end.

%% SECURITY: BIF allowlist — only these module:function/arity combinations are permitted
%% TIGHTENED: Removed higher-order functions (map, filter, foldl, foldr, foreach)
is_allowed_bif(erlang, '+', 2) -> true;
is_allowed_bif(erlang, '-', 2) -> true;
is_allowed_bif(erlang, '*', 2) -> true;
is_allowed_bif(erlang, '/', 2) -> true;
is_allowed_bif(erlang, 'div', 2) -> true;
is_allowed_bif(erlang, 'rem', 2) -> true;
is_allowed_bif(erlang, '==', 2) -> true;
is_allowed_bif(erlang, '/=', 2) -> true;
is_allowed_bif(erlang, '<', 2) -> true;
is_allowed_bif(erlang, '>', 2) -> true;
is_allowed_bif(erlang, '=<', 2) -> true;
is_allowed_bif(erlang, '>=', 2) -> true;
is_allowed_bif(erlang, 'not', 1) -> true;
is_allowed_bif(erlang, display, 1) -> true;
is_allowed_bif(io, format, 1) -> true;
is_allowed_bif(io, format, 2) -> true;
%% lists: only first-order safe functions
is_allowed_bif(lists, reverse, 1) -> true;
is_allowed_bif(lists, append, 2) -> true;
is_allowed_bif(lists, nth, 2) -> true;
is_allowed_bif(lists, length, 1) -> true;
is_allowed_bif(lists, member, 2) -> true;
is_allowed_bif(lists, keyfind, 3) -> true;
is_allowed_bif(lists, keystore, 4) -> true;
is_allowed_bif(lists, keydelete, 3) -> true;
is_allowed_bif(lists, sort, 1) -> true;
is_allowed_bif(lists, usort, 1) -> true;
is_allowed_bif(lists, flatten, 1) -> true;
is_allowed_bif(lists, zip, 2) -> true;
is_allowed_bif(lists, unzip, 1) -> true;
is_allowed_bif(lists, last, 1) -> true;
is_allowed_bif(lists, sum, 1) -> true;
is_allowed_bif(lists, max, 1) -> true;
is_allowed_bif(lists, min, 1) -> true;
%% FINDING 7 FIX: Add special wrapper for BIFs that need argument caps
is_allowed_bif(lists, seq, 2) -> capped;
is_allowed_bif(lists, duplicate, 2) -> capped;
is_allowed_bif(maps, new, 0) -> true;
is_allowed_bif(maps, get, 2) -> true;
is_allowed_bif(maps, put, 3) -> true;
is_allowed_bif(maps, remove, 2) -> true;
is_allowed_bif(maps, keys, 1) -> true;
is_allowed_bif(maps, values, 1) -> true;
is_allowed_bif(math, sin, 1) -> true;
is_allowed_bif(math, cos, 1) -> true;
is_allowed_bif(math, sqrt, 1) -> true;
is_allowed_bif(math, pow, 2) -> true;
is_allowed_bif(string, length, 1) -> true;
is_allowed_bif(string, concat, 2) -> true;
is_allowed_bif(_, _, _) -> false.
