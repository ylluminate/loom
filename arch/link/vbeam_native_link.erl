%%% @doc Internal linker that resolves relocations and patches machine code.
%%% Manages a symbol table and relocation entries, resolving them against
%%% final virtual addresses for text, data, and BSS sections.
%%% @end
-module(vbeam_native_link).

-export([
    new/0,
    add_symbol/5,
    add_reloc/5,
    resolve/4,
    apply_patches/2,
    resolve_labels/1
]).

%% ============================================================================
%% Types
%% ============================================================================

-record(reloc, {
    offset :: non_neg_integer(),     % byte offset in code where to patch
    symbol :: binary(),               % symbol name to resolve
    type :: rel32 | abs64 | abs32s | adrp_page21 | add_pageoff12,
    addend :: integer()               % addend for the relocation
}).

-record(symbol, {
    name :: binary(),
    offset :: non_neg_integer(),   % byte offset within its section
    section :: text | data | bss,  % which section
    exported :: boolean()
}).

-type linker_state() :: #{
    symbols := [#symbol{}],
    relocs := [#reloc{}]
}.

-type patch() :: {Offset :: non_neg_integer(), Size :: pos_integer(), Value :: integer()}.

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Create a new empty linker state.
-spec new() -> linker_state().
new() ->
    #{symbols => [], relocs => []}.

%% @doc Add a symbol to the linker state.
-spec add_symbol(linker_state(), binary(), non_neg_integer(),
                 text | data | bss, boolean()) -> linker_state().
add_symbol(#{symbols := Syms} = State, Name, Offset, Section, Exported) ->
    Sym = #symbol{
        name = Name,
        offset = Offset,
        section = Section,
        exported = Exported
    },
    State#{symbols := [Sym | Syms]}.

%% @doc Add a relocation entry to the linker state.
-spec add_reloc(linker_state(), non_neg_integer(), binary(),
                rel32 | abs64 | abs32s | adrp_page21 | add_pageoff12
                | arm64_branch26 | arm64_cond_branch19, integer()) -> linker_state().
add_reloc(#{relocs := Relocs} = State, Offset, Symbol, Type, Addend) ->
    Reloc = #reloc{
        offset = Offset,
        symbol = Symbol,
        type = Type,
        addend = Addend
    },
    State#{relocs := [Reloc | Relocs]}.

%% @doc Resolve all relocations against final section base addresses.
%% Returns a list of patches to apply to the code binary.
%% TextBase, DataBase, BssBase: virtual addresses of each section.
-spec resolve(linker_state(), non_neg_integer(), non_neg_integer(),
              non_neg_integer()) ->
    {ok, [patch()]} | {error, {unresolved_symbols, [binary()]}}.
resolve(#{symbols := Syms, relocs := Relocs}, TextBase, DataBase, BssBase) ->
    %% Build symbol address lookup map
    SymMap = build_sym_map(Syms, TextBase, DataBase, BssBase),

    %% Resolve each relocation
    resolve_relocs(Relocs, SymMap, TextBase, []).

%% @doc Apply a list of patches to a code binary.
%% Patches: {Offset, Size, Value} for simple byte writes,
%%          {Offset, {adrp, PageDelta}} for ARM64 ADRP instruction patching,
%%          {Offset, {add_imm12, PageOff}} for ARM64 ADD imm12 patching.
-spec apply_patches(binary(), [patch()]) -> binary().
apply_patches(Code, []) ->
    Code;
apply_patches(Code, [{Offset, adrp, PageDelta} | Rest]) ->
    Patched = patch_adrp(Code, Offset, PageDelta),
    apply_patches(Patched, Rest);
apply_patches(Code, [{Offset, add_imm12, PageOff} | Rest]) ->
    Patched = patch_add_imm12(Code, Offset, PageOff),
    apply_patches(Patched, Rest);
apply_patches(Code, [{Offset, arm64_b26, ByteOffset} | Rest]) ->
    Patched = patch_arm64_branch26(Code, Offset, ByteOffset),
    apply_patches(Patched, Rest);
apply_patches(Code, [{Offset, arm64_bcond19, ByteOffset} | Rest]) ->
    Patched = patch_arm64_cond_branch19(Code, Offset, ByteOffset),
    apply_patches(Patched, Rest);
apply_patches(Code, [{Offset, Size, Value} | Rest]) ->
    Patched = patch_binary(Code, Offset, Size, Value),
    apply_patches(Patched, Rest).

%% @doc Resolve labels in a list of instructions.
%% Takes a list of {label, Name} atoms and encoded instruction binaries.
%% Returns {ResolvedCode, Symbols} where Symbols maps label names to offsets.
-spec resolve_labels([{label, binary()} | binary()]) ->
    {binary(), #{binary() => non_neg_integer()}}.
resolve_labels(Instructions) ->
    %% First pass: compute offsets of each label
    {LabelMap, TotalSize} = compute_label_offsets(Instructions, 0, #{}),

    %% Second pass: resolve branch references and concatenate code
    {ResolvedParts, _} = resolve_instructions(Instructions, LabelMap, 0, []),

    Code = iolist_to_binary(lists:reverse(ResolvedParts)),
    %% Sanity check
    TotalSize = byte_size(Code),
    {Code, LabelMap}.

%% ============================================================================
%% Internal: Symbol Map
%% ============================================================================

%% @doc Build a map from symbol name to absolute virtual address.
-spec build_sym_map([#symbol{}], non_neg_integer(), non_neg_integer(),
                    non_neg_integer()) -> #{binary() => non_neg_integer()}.
build_sym_map(Syms, TextBase, DataBase, BssBase) ->
    lists:foldl(
        fun(#symbol{name = Name, offset = Off, section = Sec}, Acc) ->
            Base = section_base(Sec, TextBase, DataBase, BssBase),
            Acc#{Name => Base + Off}
        end,
        #{},
        Syms).

section_base(text, TextBase, _DataBase, _BssBase) -> TextBase;
section_base(data, _TextBase, DataBase, _BssBase) -> DataBase;
section_base(bss, _TextBase, _DataBase, BssBase) -> BssBase.

%% ============================================================================
%% Internal: Relocation Resolution
%% ============================================================================

resolve_relocs([], _SymMap, _TextBase, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_relocs([#reloc{offset = Off, symbol = Sym, type = Type,
                       addend = Addend} | Rest],
               SymMap, TextBase, Acc) ->
    case maps:find(Sym, SymMap) of
        {ok, SymAddr} ->
            Patch = compute_patch(Type, Off, SymAddr, TextBase, Addend),
            resolve_relocs(Rest, SymMap, TextBase, [Patch | Acc]);
        error ->
            %% Collect all unresolved symbols
            Unresolved = collect_unresolved(Rest, SymMap, [Sym]),
            {error, {unresolved_symbols, Unresolved}}
    end.

%% Collect all unresolved symbols for a complete error message.
collect_unresolved([], _SymMap, Acc) ->
    lists:reverse(lists:usort(Acc));
collect_unresolved([#reloc{symbol = Sym} | Rest], SymMap, Acc) ->
    case maps:is_key(Sym, SymMap) of
        true -> collect_unresolved(Rest, SymMap, Acc);
        false -> collect_unresolved(Rest, SymMap, [Sym | Acc])
    end.

%% @doc Compute a single patch from a relocation entry.
-spec compute_patch(atom(), non_neg_integer(),
                    non_neg_integer(), non_neg_integer(), integer()) -> patch().
compute_patch(rel32, Offset, SymAddr, TextBase, Addend) ->
    %% PC-relative 32-bit: value = target - (patch_location + 4)
    %% The +4 accounts for the 4-byte displacement field itself (x86_64 convention)
    PatchVAddr = TextBase + Offset,
    Value = SymAddr - (PatchVAddr + 4) + Addend,
    {Offset, 4, Value};

compute_patch(abs64, Offset, SymAddr, _TextBase, Addend) ->
    %% Absolute 64-bit address
    Value = SymAddr + Addend,
    {Offset, 8, Value};

compute_patch(abs32s, Offset, SymAddr, _TextBase, Addend) ->
    %% Signed 32-bit absolute address
    Value = SymAddr + Addend,
    {Offset, 4, Value};

compute_patch(adrp_page21, Offset, SymAddr, TextBase, Addend) ->
    %% ARM64 ADRP: page-relative 21-bit offset
    %% PageDelta = (target_page - pc_page) >> 12
    PatchVAddr = TextBase + Offset,
    TargetPage = (SymAddr + Addend) band (bnot 16#FFF),
    PCPage = PatchVAddr band (bnot 16#FFF),
    PageDelta = (TargetPage - PCPage) bsr 12,
    {Offset, adrp, PageDelta};

compute_patch(add_pageoff12, Offset, SymAddr, _TextBase, Addend) ->
    %% ARM64 ADD: page offset (low 12 bits of target address)
    PageOff = (SymAddr + Addend) band 16#FFF,
    {Offset, add_imm12, PageOff};

compute_patch(arm64_branch26, Offset, SymAddr, TextBase, Addend) ->
    %% ARM64 B/BL: 26-bit signed offset in instructions (offset/4)
    %% PC-relative from the instruction itself
    PatchVAddr = TextBase + Offset,
    ByteOffset = SymAddr - PatchVAddr + Addend,
    {Offset, arm64_b26, ByteOffset};

compute_patch(arm64_cond_branch19, Offset, SymAddr, TextBase, Addend) ->
    %% ARM64 B.cond: 19-bit signed offset in instructions (offset/4)
    %% PC-relative from the instruction itself
    PatchVAddr = TextBase + Offset,
    ByteOffset = SymAddr - PatchVAddr + Addend,
    {Offset, arm64_bcond19, ByteOffset}.

%% ============================================================================
%% Internal: Binary Patching
%% ============================================================================

%% @doc Patch a binary at a given offset with a little-endian value.
-spec patch_binary(binary(), non_neg_integer(), pos_integer(), integer()) -> binary().
patch_binary(Bin, Offset, 4, Value) ->
    %% Validate that Value fits in signed 32-bit before writing
    case Value >= -2147483648 andalso Value =< 2147483647 of
        true ->
            BinSize = byte_size(Bin),
            case Offset + 4 =< BinSize of
                true ->
                    <<Before:Offset/binary, _Old:4/binary, After/binary>> = Bin,
                    %% Encode as signed 32-bit little-endian
                    Encoded = <<Value:32/little-signed>>,
                    <<Before/binary, Encoded/binary, After/binary>>;
                false ->
                    error({reloc_out_of_bounds, Offset, 4, BinSize})
            end;
        false ->
            error({relocation_overflow, rel32, Value})
    end;

patch_binary(Bin, Offset, 8, Value) ->
    BinSize = byte_size(Bin),
    case Offset + 8 =< BinSize of
        true ->
            <<Before:Offset/binary, _Old:8/binary, After/binary>> = Bin,
            %% Encode as 64-bit little-endian
            Encoded = <<Value:64/little-signed>>,
            <<Before/binary, Encoded/binary, After/binary>>;
        false ->
            error({reloc_out_of_bounds, Offset, 8, BinSize})
    end.

%% ARM64 ADRP instruction patching: modify immhi:immlo fields.
patch_adrp(Bin, Offset, PageDelta) ->
    BinSize = byte_size(Bin),
    %% Bounds check
    case Offset + 4 =< BinSize of
        false -> error({relocation_out_of_bounds, Offset});
        true -> ok
    end,
    %% Validate 21-bit signed range: PageDelta must fit in [-2^20, 2^20-1]
    case (PageDelta >= -(1 bsl 20)) andalso (PageDelta < (1 bsl 20)) of
        true ->
            <<Before:Offset/binary, OldInsn:32/little, After/binary>> = Bin,
            ImmLo = PageDelta band 3,
            ImmHi = (PageDelta bsr 2) band 16#7FFFF,
            Masked = OldInsn band (bnot ((3 bsl 29) bor (16#7FFFF bsl 5))),
            NewInsn = Masked bor (ImmLo bsl 29) bor (ImmHi bsl 5),
            <<Before/binary, NewInsn:32/little, After/binary>>;
        false ->
            error({adrp_overflow, PageDelta})
    end.

%% ARM64 ADD immediate patching: modify imm12 field.
patch_add_imm12(Bin, Offset, PageOff) ->
    BinSize = byte_size(Bin),
    case Offset + 4 =< BinSize of
        false -> error({relocation_out_of_bounds, Offset});
        true -> ok
    end,
    <<Before:Offset/binary, OldInsn:32/little, After/binary>> = Bin,
    Masked = OldInsn band (bnot (16#FFF bsl 10)),
    NewInsn = Masked bor ((PageOff band 16#FFF) bsl 10),
    <<Before/binary, NewInsn:32/little, After/binary>>.

%% ARM64 B/BL instruction patching: modify imm26 field.
%% B encoding:  [000101][imm26:26]
%% BL encoding: [100101][imm26:26]
%% imm26 is signed, represents offset/4 (offset in instructions).
patch_arm64_branch26(Bin, Offset, ByteOffset) ->
    BinSize = byte_size(Bin),
    %% Bounds check
    case Offset + 4 =< BinSize of
        false -> error({relocation_out_of_bounds, Offset});
        true -> ok
    end,
    %% Validate alignment
    case ByteOffset rem 4 of
        0 -> ok;
        _ -> error({arm64_branch26_misaligned, ByteOffset})
    end,
    %% Validate signed 26-bit range: offset/4 must fit in [-2^25, 2^25-1]
    %% In bytes: [-2^27, 2^27-4] = [-134217728, 134217724]
    InsnOffset = ByteOffset div 4,
    case (InsnOffset >= -(1 bsl 25)) andalso (InsnOffset < (1 bsl 25)) of
        true -> ok;
        false -> error({arm64_branch26_overflow, ByteOffset})
    end,
    <<Before:Offset/binary, OldInsn:32/little, After/binary>> = Bin,
    Imm26 = InsnOffset band 16#3FFFFFF,
    Masked = OldInsn band (bnot 16#3FFFFFF),
    NewInsn = Masked bor Imm26,
    <<Before/binary, NewInsn:32/little, After/binary>>.

%% ARM64 B.cond instruction patching: modify imm19 field.
%% B.cond encoding: [01010100][imm19:19][0][cond:4]
%% imm19 is signed, represents offset/4 (offset in instructions).
patch_arm64_cond_branch19(Bin, Offset, ByteOffset) ->
    %% Bounds check: ensure we have 4 bytes at Offset
    BinSize = byte_size(Bin),
    case Offset + 4 =< BinSize of
        false -> error({relocation_out_of_bounds, Offset});
        true -> ok
    end,
    %% Validate alignment
    case ByteOffset rem 4 of
        0 -> ok;
        _ -> error({arm64_cond_branch19_misaligned, ByteOffset})
    end,
    %% Validate signed 19-bit range: offset/4 must fit in [-2^18, 2^18-1]
    %% In bytes: [-2^20, 2^20-4] = [-1048576, 1048572]
    InsnOffset = ByteOffset div 4,
    case (InsnOffset >= -(1 bsl 18)) andalso (InsnOffset < (1 bsl 18)) of
        true -> ok;
        false -> error({arm64_cond_branch19_overflow, ByteOffset})
    end,
    <<Before:Offset/binary, OldInsn:32/little, After/binary>> = Bin,
    Imm19 = InsnOffset band 16#7FFFF,
    Masked = OldInsn band (bnot (16#7FFFF bsl 5)),
    NewInsn = Masked bor (Imm19 bsl 5),
    <<Before/binary, NewInsn:32/little, After/binary>>.

%% ============================================================================
%% Internal: Label Resolution
%% ============================================================================

%% First pass: compute the byte offset of each label.
compute_label_offsets([], Offset, Map) ->
    {Map, Offset};
compute_label_offsets([{label, Name} | Rest], Offset, Map) ->
    %% Labels don't consume bytes; they mark the current offset
    case maps:is_key(Name, Map) of
        true -> error({duplicate_label, Name});
        false -> compute_label_offsets(Rest, Offset, Map#{Name => Offset})
    end;
compute_label_offsets([{branch, _Target, InsnSize} | Rest], Offset, Map) ->
    %% Branch placeholder: will be patched in second pass
    compute_label_offsets(Rest, Offset + InsnSize, Map);
compute_label_offsets([Bin | Rest], Offset, Map) when is_binary(Bin) ->
    compute_label_offsets(Rest, Offset + byte_size(Bin), Map).

%% Second pass: resolve branches and build final code.
resolve_instructions([], _LabelMap, _Offset, Acc) ->
    {Acc, ok};
resolve_instructions([{label, _Name} | Rest], LabelMap, Offset, Acc) ->
    %% Labels produce no bytes
    resolve_instructions(Rest, LabelMap, Offset, Acc);
resolve_instructions([{branch, Target, InsnSize} | Rest], LabelMap, Offset, Acc) ->
    %% Resolve branch target
    case maps:find(Target, LabelMap) of
        {ok, TargetOffset} ->
            BranchBin = encode_branch(Offset, TargetOffset, InsnSize),
            resolve_instructions(Rest, LabelMap, Offset + InsnSize,
                                 [BranchBin | Acc]);
        error ->
            error({undefined_label, Target})
    end;
resolve_instructions([Bin | Rest], LabelMap, Offset, Acc) when is_binary(Bin) ->
    resolve_instructions(Rest, LabelMap, Offset + byte_size(Bin), [Bin | Acc]).

%% @doc Encode a branch instruction with resolved offset.
%% For 4-byte branches (x86_64 rel32 JMP/CALL style):
%%   displacement = target - (current + instruction_size)
%% For 4-byte branches that are ARM64-style (instruction-relative, offset/4):
%%   handled by the caller encoding the full instruction; this is the generic case.
-spec encode_branch(non_neg_integer(), non_neg_integer(), pos_integer()) -> binary().
encode_branch(CurrentOffset, TargetOffset, 4) ->
    %% x86_64 style: rel32 displacement from end of instruction
    Disp = TargetOffset - (CurrentOffset + 4),
    <<Disp:32/little-signed>>;
encode_branch(CurrentOffset, TargetOffset, 5) ->
    %% x86_64 CALL/JMP near: opcode byte + rel32
    %% The caller should have included the opcode; this encodes just displacement
    %% with a leading E8 (CALL) as default
    Disp = TargetOffset - (CurrentOffset + 5),
    <<16#E8, Disp:32/little-signed>>;
encode_branch(CurrentOffset, TargetOffset, InsnSize) when InsnSize >= 4 ->
    %% Generic: compute displacement from end of instruction
    Disp = TargetOffset - (CurrentOffset + InsnSize),
    <<Disp:32/little-signed, 0:((InsnSize - 4) * 8)>>;
encode_branch(_CurrentOffset, _TargetOffset, InsnSize) ->
    error({invalid_instruction_size, InsnSize}).
