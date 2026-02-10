-module(vbeam_native_ir).

%% IR definitions for vbeam native code generation.
%%
%% The IR is represented as plain Erlang terms (maps, tuples, atoms).
%% This module defines constructors, validators, and serialization for
%% the native code IR used by architecture-specific backends.

-export([
    new_module/1,
    new_function/3,
    add_function/2,
    add_data/4,
    add_bss/3,
    validate_module/1,
    validate_function/1,
    pretty_print/1,
    parse_file/1,
    write_file/2
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% Operand types
-type vreg()      :: {vreg, non_neg_integer()}.
-type preg()      :: {preg, atom()}.
-type stack_op()  :: {stack, integer()}.
-type imm()       :: {imm, integer()}.
-type label_ref() :: {label_ref, binary()}.
-type mem_op()    :: {mem, operand(), integer()}.
-type data_ref()  :: {data_ref, binary()}.

-type operand() :: vreg() | preg() | stack_op() | imm()
                 | label_ref() | mem_op() | data_ref().

%% Condition codes for jcc
-type condition() :: eq | ne | lt | le | gt | ge
                   | ltu | leu | gtu | geu.

%% Instruction opcodes
-type opcode() :: mov | mov_imm | add | sub | mul | sdiv | srem
                | and_ | or_ | xor_ | shl | shr | sar | neg | not_
                | load | load_byte | store | store_byte | lea
                | cmp | jmp | jcc | call | call_indirect | ret
                | push | pop
                | syscall | nop | label | raw | comment
                | print_int
                %% Phase 6a: Strings
                | string_lit | string_len | string_cmp
                | string_concat | print_str
                %% Phase 6b: Arrays
                | array_new | array_get | array_set
                | array_len | array_append
                %% Phase 6c: Structs
                | struct_new | field_get | field_set
                %% Phase 6d: Maps
                | map_new | map_get | map_put | map_delete
                %% Phase 6e: Floats
                | fadd | fsub | fmul | fdiv
                | int_to_float | float_to_int | print_float
                %% Phase 6f: Type conversions
                | int_to_str | float_to_str
                %% Phase 6g: Method calls
                | method_call.

-type instruction() :: tuple() | atom().

%% Function definition
-type native_function() :: #{
    name     := binary(),
    arity    := non_neg_integer(),
    exported := boolean(),
    params   := [operand()],
    locals   := non_neg_integer(),
    body     := [instruction()]
}.

%% Data section entry
-type data_entry() :: {Name :: binary(), Align :: pos_integer(), Bytes :: binary()}.

%% BSS section entry
-type bss_entry() :: {Name :: binary(), Align :: pos_integer(), Size :: non_neg_integer()}.

%% Target architecture
-type target() :: x86_64 | arm64.

%% Object format
-type format() :: elf64 | macho | pe.

%% Module definition
-type native_module() :: #{
    target    := target(),
    format    := format(),
    functions := [native_function()],
    data      := [data_entry()],
    bss       := [bss_entry()],
    imports   := [{binary(), non_neg_integer()}]
}.

-export_type([
    operand/0, condition/0, opcode/0, instruction/0,
    native_function/0, native_module/0,
    target/0, format/0
]).

%%--------------------------------------------------------------------
%% Constructors
%%--------------------------------------------------------------------

%% @doc Create a new empty module for the given target/format.
-spec new_module({target(), format()}) -> native_module().
new_module({Target, Format}) ->
    #{
        target    => Target,
        format    => Format,
        functions => [],
        data      => [],
        bss       => [],
        imports   => []
    }.

%% @doc Create a new function definition.
-spec new_function(binary(), non_neg_integer(), boolean()) -> native_function().
new_function(Name, Arity, Exported) when is_binary(Name),
                                         is_integer(Arity), Arity >= 0,
                                         is_boolean(Exported) ->
    #{
        name     => Name,
        arity    => Arity,
        exported => Exported,
        params   => [],
        locals   => 0,
        body     => []
    }.

%% @doc Add a function to a module.
-spec add_function(native_module(), native_function()) -> native_module().
add_function(#{functions := Fns} = Mod, Fn) ->
    Mod#{functions := Fns ++ [Fn]}.

%% @doc Add a data section entry (initialized data).
-spec add_data(native_module(), binary(), pos_integer(), binary()) -> native_module().
add_data(#{data := Data} = Mod, Name, Align, Bytes)
  when is_binary(Name), is_integer(Align), Align > 0, is_binary(Bytes) ->
    Mod#{data := Data ++ [{Name, Align, Bytes}]}.

%% @doc Add a BSS section entry (uninitialized data).
-spec add_bss(native_module(), binary(), pos_integer()) -> native_module().
add_bss(#{bss := Bss} = Mod, Name, Size)
  when is_binary(Name), is_integer(Size), Size >= 0 ->
    Mod#{bss := Bss ++ [{Name, 8, Size}]}.

%%--------------------------------------------------------------------
%% Validation
%%--------------------------------------------------------------------

%% Valid targets and formats.
-define(VALID_TARGETS, [x86_64, arm64]).
-define(VALID_FORMATS, [elf64, macho, pe]).

%% Valid instruction opcodes.
-define(VALID_OPCODES, [
    mov, mov_imm, add, sub, mul, sdiv, srem,
    and_, or_, xor_, shl, shr, sar, neg, not_,
    load, load_byte, store, store_byte, lea,
    cmp, jmp, jcc, call, call_indirect, ret,
    push, pop,
    syscall, nop, label, raw, comment,
    print_int,
    %% Phase 6: Data types
    string_lit, string_len, string_cmp, string_concat, print_str,
    array_new, array_get, array_set, array_len, array_append,
    struct_new, field_get, field_set,
    map_new, map_get, map_put, map_delete,
    fadd, fsub, fmul, fdiv, int_to_float, float_to_int, print_float,
    int_to_str, float_to_str,
    method_call
]).

%% Valid condition codes for jcc.
-define(VALID_CONDITIONS, [eq, ne, lt, le, gt, ge, ltu, leu, gtu, geu]).

%% @doc Validate an entire module. Returns ok or {error, Reason}.
-spec validate_module(native_module()) -> ok | {error, term()}.
validate_module(#{target := Target, format := Format, functions := Fns,
                  data := Data, bss := Bss, imports := _Imports}) ->
    case lists:member(Target, ?VALID_TARGETS) of
        false -> {error, {invalid_target, Target}};
        true ->
            case lists:member(Format, ?VALID_FORMATS) of
                false -> {error, {invalid_format, Format}};
                true ->
                    validate_functions(Fns, Data, Bss)
            end
    end;
validate_module(_) ->
    {error, missing_required_fields}.

%% @doc Validate a single function. Returns ok or {error, Reason}.
-spec validate_function(native_function()) -> ok | {error, term()}.
validate_function(#{name := Name, arity := Arity, body := Body})
  when is_binary(Name), is_integer(Arity), Arity >= 0, is_list(Body) ->
    validate_instructions(Body);
validate_function(_) ->
    {error, invalid_function_structure}.

%%--------------------------------------------------------------------
%% Serialization
%%--------------------------------------------------------------------

%% @doc Pretty-print a module as an iolist suitable for display.
-spec pretty_print(native_module()) -> iolist().
pretty_print(Mod) ->
    io_lib:format("~p", [Mod]).

%% @doc Parse a module from an Erlang term file (file:consult format).
-spec parse_file(file:filename()) -> {ok, native_module()} | {error, term()}.
parse_file(Filename) ->
    case file:consult(Filename) of
        {ok, [Mod]} when is_map(Mod) ->
            case validate_module(Mod) of
                ok -> {ok, Mod};
                {error, _} = Err -> Err
            end;
        {ok, _} ->
            {error, {bad_term_count, Filename}};
        {error, _} = Err ->
            Err
    end.

%% @doc Write a module to an Erlang term file (file:consult format).
-spec write_file(file:filename(), native_module()) -> ok | {error, term()}.
write_file(Filename, Mod) ->
    Content = io_lib:format("~p.~n", [Mod]),
    file:write_file(Filename, Content).

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

-spec validate_functions([native_function()], [data_entry()], [bss_entry()]) ->
    ok | {error, term()}.
validate_functions([], Data, Bss) ->
    %% Validate data entries
    case validate_data_entries(Data) of
        ok -> validate_bss_entries(Bss);
        {error, _} = Err -> Err
    end;
validate_functions([Fn | Rest], Data, Bss) ->
    case validate_function(Fn) of
        ok -> validate_functions(Rest, Data, Bss);
        {error, _} = Err -> Err
    end.

validate_data_entries([]) -> ok;
validate_data_entries([{_Name, Align, _Bytes} | Rest]) ->
    case Align > 0 of
        true -> validate_data_entries(Rest);
        false -> {error, {invalid_alignment, Align, "Alignment must be > 0"}}
    end.

validate_bss_entries([]) -> ok;
validate_bss_entries([{_Name, Align, _Size} | Rest]) ->
    case Align > 0 of
        true -> validate_bss_entries(Rest);
        false -> {error, {invalid_alignment, Align, "Alignment must be > 0"}}
    end.

-spec validate_instructions([instruction()]) -> ok | {error, term()}.
validate_instructions([]) ->
    ok;
validate_instructions([Instr | Rest]) ->
    case validate_instruction(Instr) of
        ok -> validate_instructions(Rest);
        {error, _} = Err -> Err
    end.

-spec validate_instruction(instruction()) -> ok | {error, term()}.
validate_instruction(nop) -> ok;
validate_instruction(ret) -> ok;
validate_instruction(syscall) -> ok;
validate_instruction({label, Name}) when is_binary(Name) -> ok;
validate_instruction({comment, Text}) when is_binary(Text); is_list(Text) -> ok;
validate_instruction({raw, Bytes}) when is_binary(Bytes) -> ok;
validate_instruction({jmp, _Target}) -> ok;
validate_instruction({jcc, Cond, _Target}) ->
    case lists:member(Cond, ?VALID_CONDITIONS) of
        true -> ok;
        false -> {error, {invalid_condition, Cond}}
    end;
validate_instruction({call, _Target}) -> ok;
validate_instruction({call_indirect, _Target}) -> ok;
validate_instruction({push, _Op}) -> ok;
validate_instruction({pop, _Op}) -> ok;
validate_instruction({neg, _Op}) -> ok;
validate_instruction({not_, _Op}) -> ok;
validate_instruction({print_int, _Op}) -> ok;
validate_instruction({print_str, _Op}) -> ok;
validate_instruction({print_float, _Op}) -> ok;
%% Generic 2-operand instructions (map_new, array_len, etc.)
validate_instruction({Op, _Dst}) ->
    case lists:member(Op, ?VALID_OPCODES) of
        true -> ok;
        false -> {error, {invalid_opcode, Op}}
    end;
%% String opcodes: string_lit has 3 elements (op, dst, bytes)
validate_instruction({string_lit, _Dst, Bytes}) when is_binary(Bytes) -> ok;
%% Map opcodes: map_put has 5 elements
validate_instruction({map_put, _Dst, _Map, _Key, _Val}) -> ok;
%% Method call: variable arity tuple
validate_instruction({method_call, _Dst, _Type, _Method, Args}) when is_list(Args) -> ok;
%% Generic 2-operand instructions
validate_instruction({Op, _Dst, _Src}) ->
    case lists:member(Op, ?VALID_OPCODES) of
        true -> ok;
        false -> {error, {invalid_opcode, Op}}
    end;
%% Generic 3-operand instructions
validate_instruction({Op, _Dst, _Src, _Extra}) ->
    case lists:member(Op, ?VALID_OPCODES) of
        true -> ok;
        false -> {error, {invalid_opcode, Op}}
    end;
%% Generic 4-operand instructions (map_delete, etc.)
validate_instruction({Op, _A, _B, _C, _D}) ->
    case lists:member(Op, ?VALID_OPCODES) of
        true -> ok;
        false -> {error, {invalid_opcode, Op}}
    end;
validate_instruction(Instr) ->
    {error, {unrecognized_instruction, Instr}}.
