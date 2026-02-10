%% Minimal BEAM Bytecode Interpreter
%% Interprets BEAM bytecode for bootstrapping the bare-metal BEAM VM
%% Strategy: Implement only the opcode closure actually used by V-compiled programs

-module(vbeam_beam_interp).
-export([execute/3, execute/4]).

-record(proc, {
    module,         % Current module atom
    code,           % Code binary
    atoms,          % Atom table (list)
    strings,        % String table (binary)
    imports,        % Import table
    exports,        % Export table
    literals,       % Literal table
    labels,         % Label -> PC mapping
    x = #{},        % X registers (x0-x255)
    y = [],         % Y registers (stack frame) - list for easy push/pop
    pc = 0,         % Program counter
    stack = [],     % Call stack (return addresses)
    heap = []       % Heap (for cons cells, tuples)
}).

%% Execute a function in a loaded BEAM module
execute(Chunks, FunctionName, Args) ->
    execute(Chunks, FunctionName, Args, []).

execute(Chunks, FunctionName, Args, Options) ->
    %% Initialize process state
    Proc = init_proc(Chunks),

    %% Find the function's entry point
    case find_function_label(Proc, FunctionName, length(Args)) of
        {ok, Label} ->
            %% Set up initial X registers with arguments
            InitX = lists:foldl(
                fun({Arg, Index}, Acc) ->
                    Acc#{Index => Arg}
                end,
                #{},
                lists:zip(Args, lists:seq(0, length(Args) - 1))
            ),

            %% Find PC for label
            PC = maps:get(Label, Proc#proc.labels, 0),

            %% Run the interpreter
            Proc2 = Proc#proc{x = InitX, pc = PC},
            run(Proc2, Options);
        error ->
            {error, {function_not_found, FunctionName, length(Args)}}
    end.

%% Initialize process state from parsed chunks
init_proc(Chunks) ->
    %% Extract required chunks
    Code = maps:get('Code', Chunks),
    CodeBinary = maps:get(code, Code),

    %% Get atom table
    Atoms = case maps:get('AtU8', Chunks, undefined) of
        undefined ->
            case maps:get('Atom', Chunks, undefined) of
                undefined -> [];
                {AtomList, _} -> AtomList
            end;
        {AtomList, _} -> AtomList
    end,

    %% Get other tables
    Strings = maps:get('StrT', Chunks, <<>>),
    Imports = maps:get('ImpT', Chunks, []),
    Exports = maps:get('ExpT', Chunks, []),
    Literals = maps:get('LitT', Chunks, []),

    %% Build label map
    Labels = build_label_map(CodeBinary),

    #proc{
        code = CodeBinary,
        atoms = Atoms,
        strings = Strings,
        imports = Imports,
        exports = Exports,
        literals = Literals,
        labels = Labels
    }.

%% Build label -> PC mapping
build_label_map(Code) ->
    build_label_map(Code, 0, #{}).

build_label_map(<<>>, _PC, Acc) ->
    Acc;
build_label_map(Code, PC, Acc) ->
    case decode_instruction(Code, PC) of
        {label, Label, NextPC} ->
            build_label_map(Code, NextPC, Acc#{Label => PC});
        {_, _, NextPC} ->
            build_label_map(Code, NextPC, Acc);
        done ->
            Acc
    end.

%% Find function entry label
find_function_label(#proc{exports = Exports, atoms = Atoms}, FunName, Arity) ->
    %% Safely convert to atom - only if it already exists
    FunAtom = case FunName of
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

    case FunAtom of
        undefined ->
            error;
        _ ->
            case lists:keyfind(FunAtom, 1, lists:zip(Atoms, lists:seq(1, length(Atoms)))) of
                {FunAtom, FunIndex} ->
                    case lists:keyfind({FunIndex, Arity, '_'}, 1,
                                       [{F, A, L} || {F, A, L} <- Exports]) of
                        false ->
                            %% Try without label wildcard
                            case [L || {F, A, L} <- Exports, F =:= FunIndex, A =:= Arity] of
                                [Label | _] -> {ok, Label};
                                [] -> error
                            end;
                        {_, _, Label} ->
                            {ok, Label}
                    end;
                false ->
                    error
            end
    end.

%% Main interpreter loop
run(Proc, Options) ->
    case decode_and_execute(Proc, Options) of
        {continue, Proc2} ->
            run(Proc2, Options);
        {return, Value, _Proc2} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% Decode and execute one instruction
decode_and_execute(#proc{code = Code, pc = PC} = Proc, Options) ->
    case decode_instruction(Code, PC) of
        done ->
            {return, ok, Proc};

        {return, _NextPC} ->
            %% Return from function
            case Proc#proc.stack of
                [] ->
                    %% Return from top-level function
                    RetVal = maps:get(0, Proc#proc.x, undefined),
                    {return, RetVal, Proc};
                [RetAddr | RestStack] ->
                    {continue, Proc#proc{pc = RetAddr, stack = RestStack}}
            end;

        {move, Src, Dst, NextPC} ->
            %% Move value from Src to Dst
            Value = get_value(Src, Proc),
            Proc2 = set_register(Dst, Value, Proc),
            {continue, Proc2#proc{pc = NextPC}};

        {call, _Arity, Label, NextPC} ->
            %% Call function at Label
            case maps:find(Label, Proc#proc.labels) of
                {ok, TargetPC} ->
                    Proc2 = Proc#proc{
                        pc = TargetPC,
                        stack = [NextPC | Proc#proc.stack]
                    },
                    {continue, Proc2};
                error ->
                    {error, {label_not_found, Label}}
            end;

        {call_ext, Arity, Import, NextPC} ->
            %% Call external function
            case execute_bif(Import, Arity, Proc, Options) of
                {ok, Result, Proc2} ->
                    Proc3 = set_register({x, 0}, Result, Proc2),
                    {continue, Proc3#proc{pc = NextPC}};
                {error, Reason} ->
                    {error, Reason}
            end;

        {allocate, StackNeed, _Live, NextPC} ->
            %% Allocate stack frame
            Proc2 = Proc#proc{
                y = lists:duplicate(StackNeed, undefined) ++ Proc#proc.y,
                pc = NextPC
            },
            {continue, Proc2};

        {deallocate, N, NextPC} ->
            %% Deallocate stack frame
            Proc2 = Proc#proc{
                y = lists:nthtail(N, Proc#proc.y),
                pc = NextPC
            },
            {continue, Proc2};

        {test_heap, _Need, _Live, NextPC} ->
            %% Heap allocation test (no-op for now)
            {continue, Proc#proc{pc = NextPC}};

        {label, _Label, NextPC} ->
            %% Label - just advance PC
            {continue, Proc#proc{pc = NextPC}};

        {func_info, _Module, _Function, _Arity, NextPC} ->
            %% Function info - just advance PC
            {continue, Proc#proc{pc = NextPC}};

        {int_code_end, _NextPC} ->
            %% End of code
            {return, ok, Proc};

        {UnknownOp, NextPC} when is_atom(UnknownOp) ->
            %% Unknown opcode - skip for now
            io:format("Warning: Unimplemented opcode ~p at PC ~p~n", [UnknownOp, PC]),
            {continue, Proc#proc{pc = NextPC}};

        {error, Reason} ->
            {error, Reason}
    end.

%% Decode a single instruction
%% Note: This is a simplified decoder - real BEAM uses compact encoding
decode_instruction(Code, PC) when PC >= byte_size(Code) ->
    done;
decode_instruction(Code, PC) ->
    case Code of
        <<_:PC/binary, Opcode:8, Rest/binary>> ->
            decode_opcode(Opcode, Rest, PC + 1);
        _ ->
            done
    end.

%% Decode opcodes (simplified - real BEAM uses variable-length encoding)
decode_opcode(1, Rest, PC) ->
    %% label
    {Label, _Rest2, NextPC} = decode_int(Rest, PC),
    {label, Label, NextPC};

decode_opcode(2, Rest, PC) ->
    %% func_info
    {Module, Rest2, PC2} = decode_int(Rest, PC),
    {Function, Rest3, PC3} = decode_int(Rest2, PC2),
    {Arity, _Rest4, NextPC} = decode_int(Rest3, PC3),
    {func_info, Module, Function, Arity, NextPC};

decode_opcode(3, _Rest, PC) ->
    %% int_code_end
    {int_code_end, PC};

decode_opcode(4, Rest, PC) ->
    %% call
    {Arity, Rest2, PC2} = decode_int(Rest, PC),
    {Label, _Rest3, NextPC} = decode_int(Rest2, PC2),
    {call, Arity, Label, NextPC};

decode_opcode(19, _Rest, PC) ->
    %% return
    {return, PC};

decode_opcode(64, Rest, PC) ->
    %% move
    {Src, Rest2, PC2} = decode_arg(Rest, PC),
    {Dst, _Rest3, NextPC} = decode_arg(Rest2, PC2),
    {move, Src, Dst, NextPC};

decode_opcode(10, Rest, PC) ->
    %% allocate
    {StackNeed, Rest2, PC2} = decode_int(Rest, PC),
    {Live, _Rest3, NextPC} = decode_int(Rest2, PC2),
    {allocate, StackNeed, Live, NextPC};

decode_opcode(18, Rest, PC) ->
    %% deallocate
    {N, _Rest2, NextPC} = decode_int(Rest, PC),
    {deallocate, N, NextPC};

decode_opcode(59, Rest, PC) ->
    %% test_heap
    {Need, Rest2, PC2} = decode_int(Rest, PC),
    {Live, _Rest3, NextPC} = decode_int(Rest2, PC2),
    {test_heap, Need, Live, NextPC};

decode_opcode(78, Rest, PC) ->
    %% call_ext
    {Arity, Rest2, PC2} = decode_int(Rest, PC),
    {Import, _Rest3, NextPC} = decode_int(Rest2, PC2),
    {call_ext, Arity, Import, NextPC};

decode_opcode(Op, _Rest, PC) ->
    %% Unknown opcode
    io:format("Unknown opcode ~p at PC ~p~n", [Op, PC]),
    {unknown_opcode, PC + 1}.

%% Decode integer (simplified - just read one byte)
%% Returns {Value, Rest2} where Rest2 is remaining bytes
decode_int(<<Value:8, Rest2/binary>>, PC) ->
    {Value, Rest2, PC + 1};
decode_int(_Rest, _PC) ->
    {error, truncated_operand}.

%% Decode argument (register, literal, etc.)
%% Returns {Arg, Rest2} where Rest2 is remaining bytes
decode_arg(<<Tag:4, Value:4, Rest2/binary>>, PC) ->
    Arg = case Tag of
        0 -> {x, Value};  % X register
        1 -> {y, Value};  % Y register
        2 -> {atom, Value};  % Atom
        3 -> {integer, Value};  % Small integer
        4 -> {literal, Value};  % Literal
        _ -> {unknown, Value}
    end,
    {Arg, Rest2, PC + 1};
decode_arg(_Rest, _PC) ->
    {error, truncated_operand}.

%% Get value from source
get_value({x, N}, #proc{x = X}) ->
    maps:get(N, X, undefined);
get_value({y, N}, #proc{y = Y}) ->
    lists:nth(N + 1, Y);
get_value({atom, N}, #proc{atoms = Atoms}) ->
    lists:nth(N + 1, Atoms);
get_value({integer, N}, _Proc) ->
    N;
get_value({literal, N}, #proc{literals = Literals}) ->
    lists:nth(N + 1, Literals);
get_value(Value, _Proc) when is_integer(Value); is_atom(Value); is_binary(Value) ->
    Value;
get_value(_Other, _Proc) ->
    undefined.

%% Set register value
set_register({x, N}, Value, #proc{x = X} = Proc) ->
    Proc#proc{x = X#{N => Value}};
set_register({y, N}, Value, #proc{y = Y} = Proc) ->
    Y2 = lists:sublist(Y, N) ++ [Value] ++ lists:nthtail(N + 1, Y),
    Proc#proc{y = Y2};
set_register(_Other, _Value, Proc) ->
    Proc.

%% Execute built-in function
execute_bif(ImportIndex, Arity, #proc{imports = Imports, x = X} = Proc, Options) ->
    case lists:nth(ImportIndex + 1, Imports) of
        {ModIndex, FunIndex, Arity} ->
            Mod = lists:nth(ModIndex, Proc#proc.atoms),
            Fun = lists:nth(FunIndex, Proc#proc.atoms),

            %% Get arguments from X registers
            Args = [maps:get(I, X, undefined) || I <- lists:seq(0, Arity - 1)],

            %% Execute the BIF
            case execute_erlang_bif(Mod, Fun, Args, Options) of
                {ok, Result} ->
                    {ok, Result, Proc};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, invalid_import}
    end.

%% Execute Erlang BIFs
execute_erlang_bif(erlang, display, [Arg], Options) ->
    case proplists:get_value(quiet, Options, false) of
        false -> io:format("~p~n", [Arg]);
        true -> ok
    end,
    {ok, ok};

execute_erlang_bif(erlang, '+', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A + B};

execute_erlang_bif(erlang, '-', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A - B};

execute_erlang_bif(erlang, '*', [A, B], _Options) when is_number(A), is_number(B) ->
    {ok, A * B};

execute_erlang_bif(erlang, 'div', [A, B], _Options) when is_integer(A), is_integer(B) ->
    {ok, A div B};

execute_erlang_bif(erlang, '==', [A, B], _Options) ->
    {ok, A == B};

execute_erlang_bif(erlang, '/=', [A, B], _Options) ->
    {ok, A /= B};

execute_erlang_bif(erlang, '<', [A, B], _Options) ->
    {ok, A < B};

execute_erlang_bif(erlang, '>', [A, B], _Options) ->
    {ok, A > B};

execute_erlang_bif(erlang, '=<', [A, B], _Options) ->
    {ok, A =< B};

execute_erlang_bif(erlang, '>=', [A, B], _Options) ->
    {ok, A >= B};

execute_erlang_bif(io, format, [Format], _Options) ->
    io:format("~s", [Format]),
    {ok, ok};

execute_erlang_bif(io, format, [Format, Args], _Options) ->
    io:format(Format, Args),
    {ok, ok};

execute_erlang_bif(Mod, Fun, Args, _Options) ->
    io:format("Warning: BIF ~p:~p/~p not implemented~n", [Mod, Fun, length(Args)]),
    {ok, undefined}.
