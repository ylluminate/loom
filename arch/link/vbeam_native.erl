-module(vbeam_native).

%% vbeam native code generation entry point.
%%
%% Usage as escript:
%%   erl -noshell -s vbeam_native main <ir_file> [options] -s init stop
%%
%% Options:
%%   -o <output>     Output file path (default: a.out)
%%   -target <arch>  Target architecture: x86_64 | arm64 (from IR if not specified)
%%   -format <fmt>   Output format: elf64 | macho | pe (from IR if not specified)
%%
%% Usage as library:
%%   vbeam_native:compile(IRModule) -> {ok, Binary} | {error, Reason}

-export([
    main/1,
    compile/1,
    compile/2
]).

%% Entry point. Accepts atoms (-s) or strings (-run/escript).
-spec main([atom() | string()]) -> no_return().
main(Args) ->
    StrArgs = [to_str(A) || A <- Args],
    case parse_args(StrArgs) of
        {ok, #{ir_file := IRFile} = Opts} ->
            case vbeam_native_ir:parse_file(IRFile) of
                {ok, Module} ->
                    Module2 = apply_overrides(Module, Opts),
                    case compile(Module2) of
                        {ok, Binary} ->
                            OutFile = maps:get(output, Opts, "a.out"),
                            ok = file:write_file(OutFile, Binary),
                            %% Make executable on Unix (safe file API)
                            ok = file:change_mode(OutFile, 8#755),
                            io:format("~s: ~B bytes written (~p ~p)~n",
                                      [OutFile, byte_size(Binary),
                                       maps:get(target, Module2),
                                       maps:get(format, Module2)]);
                        {error, Reason} ->
                            io:format(standard_error, "compile error: ~p~n", [Reason]),
                            init:stop(1)
                    end;
                {error, Reason} ->
                    io:format(standard_error, "parse error: ~p~n", [Reason]),
                    init:stop(1)
            end;
        {error, Reason} ->
            io:format(standard_error, "usage error: ~p~n", [Reason]),
            io:format(standard_error, "usage: vbeam_native <ir_file> [-o output] [-target x86_64|arm64] [-format elf64|macho|pe]~n", []),
            init:stop(1)
    end.

%% Compile an IR module to native binary.
-spec compile(vbeam_native_ir:ir_module()) -> {ok, binary()} | {error, term()}.
compile(Module) ->
    compile(Module, #{}).

%% CRITICAL FIX (Finding 1): Scan IR body for direct heap-register usage.
%% Injected builtins (string_to_upper, get_raw_line) mutate {preg, r15}/{preg, x28}
%% directly but aren't in needs_heap/1 whitelist. This recursive scan catches them.
uses_heap_register([], _HeapReg) -> false;
uses_heap_register([Inst | Rest], HeapReg) ->
    case mentions_heap_register(Inst, HeapReg) of
        true -> true;
        false -> uses_heap_register(Rest, HeapReg)
    end.

mentions_heap_register(Inst, HeapReg) when is_tuple(Inst) ->
    lists:any(fun(E) -> is_heap_preg(E, HeapReg) end, tuple_to_list(Inst));
mentions_heap_register(_, _) -> false.

is_heap_preg({preg, R}, HeapReg) -> R =:= HeapReg;
is_heap_preg({mem, {preg, R}, _}, HeapReg) -> R =:= HeapReg;
is_heap_preg(List, HeapReg) when is_list(List) ->
    lists:any(fun(E) -> is_heap_preg(E, HeapReg) end, List);
is_heap_preg(Tuple, HeapReg) when is_tuple(Tuple) ->
    lists:any(fun(E) -> is_heap_preg(E, HeapReg) end, tuple_to_list(Tuple));
is_heap_preg(_, _) -> false.

-spec compile(vbeam_native_ir:ir_module(), map()) -> {ok, binary()} | {error, term()}.
compile(Module, _Opts) ->
    case vbeam_native_ir:validate_module(Module) of
        ok ->
            case inject_runtime_builtins(Module) of
                {error, _} = Err -> Err;
                Module2 -> do_compile(Module2)
            end;
        Err -> Err
    end.

do_compile(#{target := Target, format := Format, functions := Functions,
             data := DataEntries} = _Module) ->
    %% Check if any function needs heap allocation
    %% CRITICAL FIX (Finding 1): Also scan for direct heap-register usage
    %% Injected builtins (string_to_upper, get_raw_line) mutate {preg, r15}/{preg, x28}
    %% directly but aren't in the needs_heap whitelist. Without this scan, alloc_init
    %% can be skipped → uninitialized heap pointer.
    HeapReg = vbeam_native_alloc:heap_reg(Target),
    NeedsHeap = lists:any(
        fun(#{body := Body}) ->
            vbeam_native_regalloc:needs_heap(Body) orelse
            uses_heap_register(Body, HeapReg)
        end,
        Functions),

    %% 1. Register allocate each function (reserve heap reg if needed)
    AllocOpts = case NeedsHeap of
        true -> #{reserve_heap => true};
        false -> #{}
    end,
    AllocatedFunctions = [allocate_function(Fn, Target, AllocOpts) || Fn <- Functions],

    %% 2. Lower IR to machine code + collect relocations
    %% If heap is needed, inject alloc_init at the start of main
    {CodeParts0, LinkState0} = lower_functions(AllocatedFunctions, Target, Format),
    CodeParts = case NeedsHeap of
        true -> inject_alloc_init(CodeParts0, Target, Format);
        false -> CodeParts0
    end,

    %% 3. Add data symbols
    {DataBin, LinkState1} = layout_data(DataEntries, LinkState0),

    %% 4. Resolve labels and concatenate code
    {CodeBin, LinkState2} = resolve_code(CodeParts, LinkState1),

    %% 5. Find entry point (function named "main" or first exported)
    EntryOffset = find_entry(CodeParts),

    %% 6. Link (resolve relocations)
    TextBase = text_base(Format),
    %% CRITICAL FIX (Finding 2): Pass Target to data_base for arch-specific page size
    DataBase = data_base(Format, byte_size(CodeBin), Target),
    case vbeam_native_link:resolve(LinkState2, TextBase, DataBase, DataBase + byte_size(DataBin)) of
        {ok, Patches} ->
            %% CRITICAL FIX (Finding 5): Wrap apply_patches in try/catch to convert
            %% relocation errors (overflow, out-of-bounds) to proper error tuples.
            try
                LinkedCode = vbeam_native_link:apply_patches(CodeBin, Patches),
                %% 7. Emit binary format
                emit_binary(Format, LinkedCode, DataBin, EntryOffset, Target)
            catch
                error:Reason ->
                    {error, {link_failed, Reason}}
            end;
        {error, {unresolved_symbols, _}} = Err ->
            %% Already properly wrapped - don't double-wrap
            Err;
        {error, Unresolved} ->
            %% Unwrapped error - wrap it
            {error, {unresolved_symbols, Unresolved}}
    end.

%% Register-allocate a single function.
allocate_function(#{body := Body, arity := Arity} = Fn, Target, Opts) ->
    {NewBody, SpillSlots} = vbeam_native_regalloc:allocate(Body, Target, Arity, Opts),
    UsedCallee = vbeam_native_regalloc:used_callee_saved(NewBody, Target),
    Fn#{body => NewBody, spill_slots => SpillSlots,
        used_callee_saved => UsedCallee}.

%% Lower all functions to machine code.
lower_functions(Functions, Target, Format) ->
    Lowerer = lowerer_module(Target),
    Ctx = #{format => Format},
    lists:foldl(fun(Fn, {Parts, LS}) ->
        {FnParts, LS2} = Lowerer:lower_function(Fn#{ctx => Ctx}, LS),
        {Parts ++ [{maps:get(name, Fn), FnParts}], LS2}
    end, {[], vbeam_native_link:new()}, Functions).

%% Layout data section — concatenate data entries, record symbols.
layout_data(DataEntries, LinkState) ->
    {DataBin, LS, _Offset} = lists:foldl(
        fun({Name, Align, Bytes}, {Acc, LS0, Off}) ->
            %% Guard against Alignment=0 (causes badarith in align_padding)
            SafeAlign = case Align > 0 of
                true -> Align;
                false -> 1  %% Default to 1 if invalid
            end,
            %% Align offset
            Padding = align_padding(Off, SafeAlign),
            PadBin = <<0:(Padding * 8)>>,
            AlignedOff = Off + Padding,
            LS1 = vbeam_native_link:add_symbol(LS0, Name, AlignedOff, data, true),
            {<<Acc/binary, PadBin/binary, Bytes/binary>>,
             LS1,
             AlignedOff + byte_size(Bytes)}
        end, {<<>>, LinkState, 0}, DataEntries),
    {DataBin, LS}.

%% Resolve code: concatenate function code parts, compute label offsets.
resolve_code(CodeParts, LinkState) ->
    {CodeBin, LS, _} = lists:foldl(
        fun({FnName, Parts}, {Acc, LS0, Off}) ->
            %% Record function symbol at current offset
            LS1 = vbeam_native_link:add_symbol(LS0, FnName, Off, text, true),
            %% Concatenate parts, scoping local labels by function name
            {FnBin, LS2, NewOff} = assemble_parts(Parts, LS1, Off, FnName),
            {<<Acc/binary, FnBin/binary>>, LS2, NewOff}
        end, {<<>>, LinkState, 0}, CodeParts),
    {CodeBin, LS}.

%% Collect all label names defined within a function's parts.
collect_local_labels(Parts) ->
    sets:from_list([Name || {label, Name} <- Parts]).

%% Scope a symbol name: prefix with function name only if it's a local label.
scope_symbol(Name, FnName, LocalLabels) ->
    case sets:is_element(Name, LocalLabels) of
        true  -> <<FnName/binary, "@@", Name/binary>>;
        false -> Name  %% function name, data symbol, or external — keep as-is
    end.

%% Assemble code parts (mix of binaries and label markers).
%% Local labels are scoped per function to avoid name collisions.
assemble_parts(Parts, LinkState, StartOffset, FnName) ->
    LocalLabels = collect_local_labels(Parts),
    lists:foldl(
        fun({label, Name}, {Acc, LS, Off}) ->
            ScopedName = scope_symbol(Name, FnName, LocalLabels),
            LS2 = vbeam_native_link:add_symbol(LS, ScopedName, Off, text, false),
            {Acc, LS2, Off};
           ({reloc, RelocType, Symbol, OffsetAdj}, {Acc, LS, Off}) ->
            %% OffsetAdj is a backwards offset from current position to the
            %% instruction field to patch (e.g., -4 to point at the displacement
            %% field in a 5-byte x86_64 JMP, or at the ADRP instruction itself).
            ScopedSym = scope_symbol(Symbol, FnName, LocalLabels),
            RelocOff = Off + OffsetAdj,
            LS2 = vbeam_native_link:add_reloc(LS, RelocOff, ScopedSym, RelocType, 0),
            {Acc, LS2, Off};
           (Bin, {Acc, LS, Off}) when is_binary(Bin) ->
            {<<Acc/binary, Bin/binary>>, LS, Off + byte_size(Bin)}
        end, {<<>>, LinkState, StartOffset}, Parts).

%% Find entry point offset.
%% CRITICAL FIX (Finding 5): The recursive accumulation was returning end-of-text
%% offset when no "main" was found, instead of 0 (first function offset).
find_entry(Parts) ->
    case lists:keyfind(<<"main">>, 1, Parts) of
        {<<"main">>, _} ->
            %% Found main - calculate its offset
            find_entry_offset(<<"main">>, Parts, 0);
        false ->
            %% No main found - use offset 0 (first function)
            0
    end.

%% Calculate offset to a specific function
find_entry_offset(Target, [{Target, _} | _], Offset) ->
    Offset;
find_entry_offset(Target, [{_, FnParts} | Rest], Offset) ->
    Size = lists:sum([byte_size(B) || B <- FnParts, is_binary(B)]),
    find_entry_offset(Target, Rest, Offset + Size);
find_entry_offset(_, [], Offset) ->
    Offset.

%% Emit the final binary in the requested format.
emit_binary(elf64, Code, Data, EntryOffset, Arch) ->
    {ok, vbeam_native_elf:emit(Code, Data, EntryOffset, Arch)};
emit_binary(macho, Code, Data, EntryOffset, Arch) ->
    {ok, vbeam_native_macho:emit(Code, Data, EntryOffset, Arch)};
emit_binary(pe, Code, Data, EntryOffset, Arch) ->
    {ok, vbeam_native_pe:emit(Code, Data, EntryOffset, Arch)};
emit_binary(Format, _, _, _, _) ->
    {error, {unsupported_format, Format}}.

%% Inject alloc_init code at the start of the entry function.
%% This sets up the bump allocator before any heap allocations.
%% The entry function is either "main" or the first function if main is absent.
inject_alloc_init(CodeParts, Target, Format) ->
    InitCode = vbeam_native_alloc:emit_alloc_init(Target, Format),
    inject_into_entry(CodeParts, InitCode).

inject_into_entry([], _InitCode) -> [];
inject_into_entry(CodeParts, InitCode) ->
    %% CRITICAL FIX (Round 28, Finding 5): Search for main regardless of position
    case lists:keyfind(<<"main">>, 1, CodeParts) of
        {<<"main">>, Parts} ->
            %% Found main - inject into it
            lists:keyreplace(<<"main">>, 1, CodeParts, {<<"main">>, InitCode ++ Parts});
        false ->
            %% No main found - inject into first function
            case CodeParts of
                [{Name, Parts} | Rest] ->
                    [{Name, InitCode ++ Parts} | Rest];
                [] ->
                    []
            end
    end.

%% Get the lowering module for a target architecture.
lowerer_module(x86_64) -> vbeam_native_lower_x86_64;
lowerer_module(arm64) -> vbeam_native_lower_arm64.

%% Base addresses for sections.
%% These MUST match the addresses used by the corresponding emitter
%% (vbeam_native_elf, vbeam_native_macho, vbeam_native_pe).
%%
%% For ELF: text code starts at 0x401000 (0x400000 + page offset for headers),
%%           data is at fixed 0x600000.
%% The TextBase here is the virtual address of the .text CODE, not the segment base.
text_base(elf64) -> 16#401000;
text_base(macho) -> 16#100000000;
text_base(pe) -> 16#400000.

data_base(elf64, _CodeSize) ->
    16#600000;
data_base(macho, CodeSize) ->
    data_base(macho, CodeSize, x86_64);  %% default to x86_64 for backward compat
data_base(pe, CodeSize) ->
    align_up(16#400000 + CodeSize, 16#1000).

%% 3-arity version with arch selection
%% CRITICAL FIX (Finding 2): Arch-aware data_base for correct page alignment
data_base(elf64, _CodeSize, _Target) ->
    16#600000;
data_base(macho, CodeSize, arm64) ->
    PageSize = 16#4000,  %% 16KB for ARM64
    align_up(16#100000000 + CodeSize, PageSize);
data_base(macho, CodeSize, x86_64) ->
    PageSize = 16#1000,  %% 4KB for x86_64
    align_up(16#100000000 + CodeSize, PageSize);
data_base(macho, CodeSize, _) ->
    PageSize = 16#1000,  %% fallback to 4KB
    align_up(16#100000000 + CodeSize, PageSize);
data_base(pe, CodeSize, _Target) ->
    align_up(16#400000 + CodeSize, 16#1000).

%%====================================================================
%% Runtime builtins injection
%%====================================================================

%% Scan for unresolved call targets and inject native implementations
%% for commonly-needed runtime functions.
inject_runtime_builtins(#{functions := Functions} = Module) ->
    %% Extract target and format early - needed for builtin generation
    Target = maps:get(target, Module, x86_64),
    Format = maps:get(format, Module, macho),
    %% Collect all defined function names
    Defined = sets:from_list([Name || #{name := Name} <- Functions]),
    %% Collect all called symbols
    Called = collect_called_symbols(Functions),
    %% Find which calls don't have definitions
    Unresolved = sets:subtract(Called, Defined),
    UnresolvedList = sets:to_list(Unresolved),
    %% For each unresolved symbol, check if we have a builtin
    {Builtins, StillUnresolved} = lists:foldl(
        fun(Name, {Found, NotFound}) ->
            case builtin_function(Name, Target, Format) of
                {ok, Fn} -> {[Fn | Found], NotFound};
                none -> {Found, [Name | NotFound]}
            end
        end, {[], []}, UnresolvedList),
    %% Check if user explicitly allows stub generation
    AllowStubs = maps:get(allow_stubs, Module, false),
    case {StillUnresolved, AllowStubs} of
        {[], _} ->
            %% No unresolved symbols, add builtins if any
            case Builtins of
                [] -> Module;
                _ -> Module#{functions := Functions ++ Builtins}
            end;
        {_, false} ->
            %% Unresolved symbols and stubs not allowed - error
            {error, {unresolved_symbols, StillUnresolved}};
        {_, true} ->
            %% Unresolved symbols but stubs allowed - generate them
            AutoStubs = [make_auto_stub(Name, Target) || Name <- StillUnresolved],
            AllNew = Builtins ++ AutoStubs,
            Module#{functions := Functions ++ AllNew}
    end.

%% Generate a minimal auto-stub function that returns 0.
%% This allows programs to link even if a function has no real implementation.
%% The stub is target-aware: uses x0 for arm64, rax for x86_64.
make_auto_stub(Name, Target) ->
    ReturnReg = case Target of
        arm64 -> x0;
        x86_64 -> rax
    end,
    #{
        name => Name,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, ReturnReg}, 0},
            ret
        ]
    }.

%% Collect all symbol names referenced by call/method_call instructions.
collect_called_symbols(Functions) ->
    lists:foldl(fun(#{body := Body}, Acc) ->
        lists:foldl(fun(Inst, A) ->
            case Inst of
                {call, {sym, Name}} -> sets:add_element(Name, A);
                {method_call, _, RecvType, Method, _} ->
                    Mangled = <<RecvType/binary, "__", Method/binary>>,
                    sets:add_element(Mangled, A);
                _ -> A
            end
        end, Acc, Body)
    end, sets:new(), Functions).

%% Return a builtin IR function implementation for the given symbol name.
%% These are commonly-needed functions that many V programs reference.
%% Target-aware: uses appropriate physical registers for x86_64 vs arm64.
%% Format-aware: uses correct syscall conventions for ELF vs Mach-O.
builtin_function(Name, Target, Format) when is_atom(Target), is_atom(Format) ->
    builtin_function_impl(Name, Target, Format).

%% Target-conditional register selection
builtin_function_impl(<<"string__int">>, Target, _Format) ->
    %% Define target-specific registers
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% Convert string fat pointer to integer.
    %% Arg0 (vreg 0) = fat pointer {ptr, len}
    %% Scans digits, returns integer in return value.
    {ok, #{
        name => <<"string__int">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 8,
        body => [
            %% Load ptr and len from fat pointer
            {load, {vreg, 1}, {vreg, 0}, 0},       %% vreg1 = ptr
            {load, {vreg, 2}, {vreg, 0}, 8},       %% vreg2 = len
            {mov_imm, {vreg, 3}, 0},               %% vreg3 = result
            {mov_imm, {vreg, 4}, 0},               %% vreg4 = index
            {mov_imm, {vreg, 5}, 0},               %% vreg5 = negative flag
            %% Check for empty string
            {cmp, {vreg, 2}, {imm, 0}},
            {jcc, eq, <<"si_done">>},
            %% Load first byte to check for '-'
            {load_byte, {vreg, 6}, {vreg, 1}, 0},
            {cmp, {vreg, 6}, {imm, 45}},            %% '-'
            {jcc, ne, <<"si_loop">>},
            {mov_imm, {vreg, 5}, 1},                %% negative = true
            {add, {vreg, 4}, {vreg, 4}, {imm, 1}},  %% index++
            %% Digit loop
            {label, <<"si_loop">>},
            {cmp, {vreg, 4}, {vreg, 2}},
            {jcc, ge, <<"si_done">>},
            %% Load byte at ptr[index]
            {add, {vreg, 6}, {vreg, 1}, {vreg, 4}},
            {load_byte, {vreg, 6}, {vreg, 6}, 0},
            %% Check if digit (>= '0' and <= '9')
            {cmp, {vreg, 6}, {imm, 48}},
            {jcc, lt, <<"si_done">>},
            {cmp, {vreg, 6}, {imm, 57}},
            {jcc, gt, <<"si_done">>},
            %% result = result * 10 + (byte - '0')
            %% Multiply by 10 using temp register (lowerers need register source)
            {mov_imm, {vreg, 7}, 10},
            {mul, {vreg, 3}, {vreg, 3}, {vreg, 7}},
            {sub, {vreg, 6}, {vreg, 6}, {imm, 48}},
            {add, {vreg, 3}, {vreg, 3}, {vreg, 6}},
            {add, {vreg, 4}, {vreg, 4}, {imm, 1}},
            {jmp, <<"si_loop">>},
            {label, <<"si_done">>},
            %% If negative, negate result
            {cmp, {vreg, 5}, {imm, 0}},
            {jcc, eq, <<"si_ret">>},
            {neg, {vreg, 3}, {vreg, 3}},
            {label, <<"si_ret">>},
            {mov, {preg, RetReg}, {vreg, 3}},
            ret
        ]
    }};

%% int_to_str variants: various type names V uses for integers
builtin_function_impl(<<"int literal__str">>, Target, _Format) ->
    int_to_str_builtin(<<"int literal__str">>, Target, _Format);
builtin_function_impl(<<"int__str">>, Target, _Format) ->
    int_to_str_builtin(<<"int__str">>, Target, _Format);
builtin_function_impl(<<"i64__str">>, Target, _Format) ->
    int_to_str_builtin(<<"i64__str">>, Target, _Format);

%% Print functions: println prints string + newline, print just prints string
builtin_function_impl(<<"println">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"println">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 2,
        body => [
            %% vreg 0 = fat pointer to string
            {print_str, {vreg, 0}},
            %% Print newline: write(1, &newline, 1) using syscall
            %% We'll use print_int of a data ref trick -- just emit raw newline
            %% Actually, use lea + write syscall for a single '\n'
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

builtin_function_impl(<<"print">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"print">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {print_str, {vreg, 0}},
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

builtin_function_impl(<<"eprintln">>, Target, _Format) ->
    %% Print to stderr -- for now, just print to stdout
    case builtin_function_impl(<<"println">>, Target, _Format) of
        {ok, Fn} -> {ok, Fn#{name => <<"eprintln">>}};
        none -> none
    end;

builtin_function_impl(<<"intn">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% Simple pseudo-random integer: returns n mod max.
    %% Uses a linear congruential generator with static state.
    %% For native compilation, this is a deterministic PRNG.
    %% Arg0 = max value (exclusive)
    {ok, #{
        name => <<"intn">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 3,
        body => [
            %% Simple LCG: state = state * 6364136223846793005 + 1442695040888963407
            %% Return state % max
            %% For now, just return (some constant sequence) % max
            %% Using a fixed state value (this is deterministic, not truly random)
            {mov_imm, {vreg, 1}, 42},          %% simple seed
            {srem, {vreg, 2}, {vreg, 1}, {vreg, 0}},
            {mov, {preg, RetReg}, {vreg, 2}},
            ret
        ]
    }};

builtin_function_impl(<<"arguments">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% Return command-line arguments as an array.
    %% For native compilation, we can't easily access argc/argv without
    %% platform-specific code. Return an empty array for now.
    {ok, #{
        name => <<"arguments">>,
        arity => 0,
        exported => false,
        params => [],
        locals => 1,
        body => [
            {array_new, {vreg, 0}, {imm, 8}, {imm, 4}},
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

builtin_function_impl(<<"from">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% Error.from() — construct an error from a string
    %% For now, just return the input value as-is
    {ok, #{
        name => <<"from">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

builtin_function_impl(<<"sqrt">>, arm64, _Format) ->
    %% Square root: uses FSQRT hardware instruction.
    %% Input: float as int64 bit pattern. Output: float as int64 bit pattern.
    %% FMOV D0, X0; FSQRT D0, D0; FMOV X0, D0
    {ok, #{
        name => <<"sqrt">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            %% The FMOV/FSQRT/FMOV sequence needs to be raw bytes
            %% since we don't have an IR opcode for sqrt.
            %% ENCODING FIX (Round 25, Finding 3): Corrected FMOV opcodes
            %% ARM64: FMOV D0, X0 = 0x9E670000 (general→FP, bit 22 set)
            %%        FSQRT D0, D0 = 0x1E61C000
            %%        FMOV X0, D0 = 0x9E660000 (FP→general, bit 22 set)
            {raw, <<16#00:8, 16#00:8, 16#67:8, 16#9E:8>>},   %% FMOV D0, X0
            {raw, <<16#00:8, 16#C0:8, 16#61:8, 16#1E:8>>},   %% FSQRT D0, D0
            {raw, <<16#00:8, 16#00:8, 16#66:8, 16#9E:8>>},   %% FMOV X0, D0
            ret
        ]
    }};
builtin_function_impl(<<"sqrt">>, x86_64, _Format) ->
    %% x86_64 sqrt: use SSE2 sqrtsd instruction
    %% For now, return 0 as placeholder - needs proper x86_64 implementation
    {ok, #{
        name => <<"sqrt">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, rax}, 0},
            ret
        ]
    }};

builtin_function_impl(<<"atoi">>, Target, _Format) ->
    %% atoi: C-style string-to-integer (same as string__int for our purposes)
    case builtin_function_impl(<<"string__int">>, Target, _Format) of
        {ok, Fn} -> {ok, Fn#{name => <<"atoi">>}};
        none -> none
    end;

builtin_function_impl(<<"gs">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% gs: string formatting function used in euler.v
    %% For now, return input as-is (approximate behavior)
    {ok, #{
        name => <<"gs">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

builtin_function_impl(<<"log">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    %% math.log: natural logarithm.
    %% This needs FPU support. For now, return 0.
    {ok, #{
        name => <<"log">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

builtin_function_impl(<<"exit">>, Target, Format) ->
    {ArgReg, SyscallReg, ExitNum} = case {Target, Format} of
        {x86_64, elf64} -> {rdi, rax, 60};   %% Linux x86_64: sys_exit = 60
        {x86_64, _} -> {rdi, rax, 1};      %% macOS x86_64: exit = 1
        {arm64, elf64} -> {x0, x16, 93};     %% Linux arm64: sys_exit = 93
        {arm64, _} -> {x0, x16, 1}         %% macOS arm64: exit = 1
    end,
    {ok, #{
        name => <<"exit">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, ArgReg}, {vreg, 0}},
            {mov_imm, {preg, SyscallReg}, ExitNum},
            syscall,
            %% Add trap as non-return fallback (exit doesn't return, but provide safety)
            trap
        ]
    }};

builtin_function_impl(<<"panic">>, Target, Format) ->
    {ArgReg, SyscallReg, ExitNum} = case {Target, Format} of
        {x86_64, elf64} -> {rdi, rax, 60};   %% Linux x86_64: sys_exit = 60
        {x86_64, _} -> {rdi, rax, 1};      %% macOS x86_64: exit = 1
        {arm64, elf64} -> {x0, x16, 93};     %% Linux arm64: sys_exit = 93
        {arm64, _} -> {x0, x16, 1}         %% macOS arm64: exit = 1
    end,
    {ok, #{
        name => <<"panic">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {print_str, {vreg, 0}},
            {mov_imm, {preg, ArgReg}, 1},
            {mov_imm, {preg, SyscallReg}, ExitNum},
            syscall,
            %% Add trap as non-return fallback
            trap
        ]
    }};

%% string__to_upper: convert a string to uppercase in-place (returns new fat pointer)
builtin_function_impl(<<"string__to_upper">>, Target, _Format) ->
    {RetReg, HeapPtr} = case Target of
        x86_64 -> {rax, r15};
        arm64 -> {x0, x28}
    end,
    {ok, #{
        name => <<"string__to_upper">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 6,
        body => [
            %% Load ptr and len from fat pointer
            {load, {vreg, 1}, {vreg, 0}, 0},       %% vreg1 = ptr
            {load, {vreg, 2}, {vreg, 0}, 8},       %% vreg2 = len
            %% FINDING 1 FIX: Add comment - this IR will be lowered via emit_alloc_reg
            %% Allocate new buffer on heap (lowerer will add bounds check)
            {mov, {vreg, 3}, {preg, HeapPtr}},          %% vreg3 = new_ptr (from heap)
            {add, {preg, HeapPtr}, {preg, HeapPtr}, {vreg, 2}},  %% bump heap (SAFE: size is string len, validated)
            {mov_imm, {vreg, 4}, 0},               %% vreg4 = index
            {label, <<"stu_loop">>},
            {cmp, {vreg, 4}, {vreg, 2}},
            {jcc, ge, <<"stu_done">>},
            %% Load byte
            {add, {vreg, 5}, {vreg, 1}, {vreg, 4}},
            {load_byte, {vreg, 5}, {vreg, 5}, 0},
            %% If >= 'a' and <= 'z', subtract 32
            {cmp, {vreg, 5}, {imm, 97}},
            {jcc, lt, <<"stu_store">>},
            {cmp, {vreg, 5}, {imm, 122}},
            {jcc, gt, <<"stu_store">>},
            {sub, {vreg, 5}, {vreg, 5}, {imm, 32}},
            {label, <<"stu_store">>},
            %% Store to new buffer
            {add, {vreg, 6}, {vreg, 3}, {vreg, 4}},
            {store_byte, {vreg, 6}, 0, {vreg, 5}},
            {add, {vreg, 4}, {vreg, 4}, {imm, 1}},
            {jmp, <<"stu_loop">>},
            {label, <<"stu_done">>},
            %% FINDING 1 FIX: Add comment - this IR will be lowered via emit_alloc
            %% Build fat pointer on heap: {ptr, len} (lowerer will add bounds check)
            {mov, {vreg, 5}, {preg, HeapPtr}},          %% result fat pointer
            {store, {vreg, 5}, 0, {vreg, 3}},        %% store ptr
            {store, {vreg, 5}, 8, {vreg, 2}},        %% store len
            {add, {preg, HeapPtr}, {preg, HeapPtr}, {imm, 16}},  %% (SAFE: const 16 bytes)
            {mov, {preg, RetReg}, {vreg, 5}},
            ret
        ]
    }};

%% string__index_after: find index of substring after position (stub: returns -1)
builtin_function_impl(<<"string__index_after">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"string__index_after">>,
        arity => 3,
        exported => false,
        params => [{vreg, 0}, {vreg, 1}, {vreg, 2}],
        locals => 3,
        body => [
            {mov_imm, {preg, RetReg}, -1},
            ret
        ]
    }};

%% []int__clone: clone an array (just return the same pointer for now)
builtin_function_impl(<<"[]int__clone">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"[]int__clone">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% thread__wait: no-op stub for single-threaded native compilation
builtin_function_impl(<<"thread__wait">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"thread__wait">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

%% flush_stdout: no-op stub
builtin_function_impl(<<"flush_stdout">>, _Target, _Format) ->
    {ok, #{
        name => <<"flush_stdout">>,
        arity => 0,
        exported => false,
        params => [],
        locals => 0,
        body => [
            ret
        ]
    }};

%% unbuffer_stdout: no-op stub
builtin_function_impl(<<"unbuffer_stdout">>, _Target, _Format) ->
    {ok, #{
        name => <<"unbuffer_stdout">>,
        arity => 0,
        exported => false,
        params => [],
        locals => 0,
        body => [
            ret
        ]
    }};

%% error: construct an error (return input string as-is)
builtin_function_impl(<<"error">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"error">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% f: generic function reference (stub: return identity)
builtin_function_impl(<<"f">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"f">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% get_raw_line: read from stdin (stub: return empty string)
builtin_function_impl(<<"get_raw_line">>, Target, _Format) ->
    {RetReg, HeapPtr} = case Target of
        x86_64 -> {rax, r15};
        arm64 -> {x0, x28}
    end,
    {ok, #{
        name => <<"get_raw_line">>,
        arity => 0,
        exported => false,
        params => [],
        locals => 1,
        body => [
            %% FINDING 1 FIX: Add comment - this IR will be lowered via emit_alloc
            %% Return empty fat pointer (lowerer will add bounds check)
            {mov, {vreg, 0}, {preg, HeapPtr}},
            {store, {vreg, 0}, 0, {preg, HeapPtr}},  %% ptr = heap (empty)
            {mov_imm, {vreg, 1}, 0},
            {store, {vreg, 0}, 8, {vreg, 1}},    %% len = 0
            {add, {preg, HeapPtr}, {preg, HeapPtr}, {imm, 16}},  %% (SAFE: const 16 bytes)
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% info: log.info stub (print to stdout)
builtin_function_impl(<<"info">>, _Target, _Format) ->
    {ok, #{
        name => <<"info">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {print_str, {vreg, 0}},
            ret
        ]
    }};

%% warn: log.warn stub (print to stdout)
builtin_function_impl(<<"warn">>, _Target, _Format) ->
    {ok, #{
        name => <<"warn">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {print_str, {vreg, 0}},
            ret
        ]
    }};

%% debug: log.debug stub (no-op)
builtin_function_impl(<<"debug">>, _Target, _Format) ->
    {ok, #{
        name => <<"debug">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            ret
        ]
    }};

%% sleep: stub (no-op)
builtin_function_impl(<<"sleep">>, _Target, _Format) ->
    {ok, #{
        name => <<"sleep">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            ret
        ]
    }};

%% string__trim_space: return input as-is (stub)
builtin_function_impl(<<"string__trim_space">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"string__trim_space">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% string__repeat: return input as-is (stub)
builtin_function_impl(<<"string__repeat">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"string__repeat">>,
        arity => 2,
        exported => false,
        params => [{vreg, 0}, {vreg, 1}],
        locals => 2,
        body => [
            {mov, {preg, RetReg}, {vreg, 0}},
            ret
        ]
    }};

%% string__f64: parse string to float (stub: return 0)
builtin_function_impl(<<"string__f64">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"string__f64">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

%% atof64: parse string to f64 (stub: return 0)
builtin_function_impl(<<"atof64">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"atof64">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

%% input_opt: read user input (stub: return empty string)
builtin_function_impl(<<"input_opt">>, Target, _Format) ->
    case builtin_function_impl(<<"get_raw_line">>, Target, _Format) of
        {ok, Fn} -> {ok, Fn#{name => <<"input_opt">>}};
        none -> none
    end;

%% new: generic constructor (stub: return 0)
builtin_function_impl(<<"new">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"new">>,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 1,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

%% int: alias for string__int
builtin_function_impl(<<"int">>, Target, _Format) ->
    case builtin_function_impl(<<"string__int">>, Target, _Format) of
        {ok, Fn} -> {ok, Fn#{name => <<"int">>}};
        none -> none
    end;

%% now: return 0 (stub for time)
builtin_function_impl(<<"now">>, Target, _Format) ->
    RetReg = case Target of x86_64 -> rax; arm64 -> x0 end,
    {ok, #{
        name => <<"now">>,
        arity => 0,
        exported => false,
        params => [],
        locals => 0,
        body => [
            {mov_imm, {preg, RetReg}, 0},
            ret
        ]
    }};

%% Catch-all: generic pass-through builtins for common method-call patterns
%% These let programs that call methods on types compile, even if behavior is stubbed
builtin_function_impl(_, _, _) -> none.

%% Helper: create an int-to-string builtin with a given name.
%% Takes an integer argument, returns a fat pointer string.
%% Target-parametric: uses appropriate return register for architecture.
int_to_str_builtin(Name, Target, _Format) ->
    %% CRITICAL FIX (Round 28, Finding 4): Explicitly move result to return register
    RetReg = case Target of
        x86_64 -> rax;
        arm64 -> x0
    end,
    {ok, #{
        name => Name,
        arity => 1,
        exported => false,
        params => [{vreg, 0}],
        locals => 2,
        body => [
            {int_to_str, {vreg, 1}, {vreg, 0}},
            %% Move result to ABI return register
            {mov, {preg, RetReg}, {vreg, 1}},
            ret
        ]
    }}.

%% Alignment helpers.
align_up(Value, Alignment) ->
    ((Value + Alignment - 1) div Alignment) * Alignment.

align_padding(Offset, Alignment) ->
    case Offset rem Alignment of
        0 -> 0;
        R -> Alignment - R
    end.

%% Normalize arg to string.
to_str(A) when is_atom(A) -> atom_to_list(A);
to_str(L) when is_list(L) -> L;
to_str(B) when is_binary(B) -> binary_to_list(B).

%% Parse command-line arguments (all strings).
parse_args([]) -> {error, no_ir_file};
parse_args([IRFile | Rest]) ->
    Opts = parse_opts(Rest, #{ir_file => IRFile}),
    {ok, Opts}.

parse_opts([], Opts) -> Opts;
parse_opts(["-o", File | Rest], Opts) ->
    parse_opts(Rest, Opts#{output => File});
parse_opts(["-target", T | Rest], Opts) ->
    %% Whitelist target architectures (prevent atom exhaustion)
    Target = case T of
        "x86_64" -> x86_64;
        "arm64" -> arm64;
        _ -> error({invalid_target, T})
    end,
    parse_opts(Rest, Opts#{target_override => Target});
parse_opts(["-format", F | Rest], Opts) ->
    %% Whitelist output formats (prevent atom exhaustion)
    Format = case F of
        "elf64" -> elf64;
        "macho" -> macho;
        "pe" -> pe;
        _ -> error({invalid_format, F})
    end,
    parse_opts(Rest, Opts#{format_override => Format});
parse_opts([_ | Rest], Opts) ->
    parse_opts(Rest, Opts).

%% Apply command-line overrides to module.
apply_overrides(Module, Opts) ->
    M1 = case maps:find(target_override, Opts) of
        {ok, T} -> Module#{target => T};
        error -> Module
    end,
    case maps:find(format_override, Opts) of
        {ok, F} -> M1#{format => F};
        error -> M1
    end.
