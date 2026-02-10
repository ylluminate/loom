%%% @doc ELF64 relocatable object loader.
%%% Parses ET_REL ELF files (Linux kernel modules .ko, object files .o).
%%% Performs symbol resolution and relocation to produce loadable code.
%%% @end
-module(vbeam_elf_loader).

-export([parse/1, resolve_symbols/2, apply_relocations/2, load/2]).

%% ============================================================================
%% ELF Constants
%% ============================================================================

-define(ELFMAG, <<16#7f, "ELF">>).
-define(ELFCLASS64, 2).
-define(ELFDATA2LSB, 1).       % Little endian
-define(EV_CURRENT, 1).
-define(ET_REL, 1).            % Relocatable object
-define(EM_X86_64, 16#3E).

%% Section header types
-define(SHT_NULL, 0).
-define(SHT_PROGBITS, 1).
-define(SHT_SYMTAB, 2).
-define(SHT_STRTAB, 3).
-define(SHT_RELA, 4).
-define(SHT_NOBITS, 8).
-define(SHT_REL, 9).

%% Section header flags
-define(SHF_WRITE, 1).
-define(SHF_ALLOC, 2).
-define(SHF_EXECINSTR, 4).

%% Symbol bindings
-define(STB_LOCAL, 0).
-define(STB_GLOBAL, 1).
-define(STB_WEAK, 2).

%% Symbol types
-define(STT_NOTYPE, 0).
-define(STT_OBJECT, 1).
-define(STT_FUNC, 2).
-define(STT_SECTION, 3).
-define(STT_FILE, 4).

%% Special section indices
-define(SHN_UNDEF, 0).
-define(SHN_ABS, 16#FFF1).
-define(SHN_COMMON, 16#FFF2).

%% x86_64 relocation types
-define(R_X86_64_NONE, 0).
-define(R_X86_64_64, 1).        % Direct 64-bit
-define(R_X86_64_PC32, 2).      % PC-relative 32-bit signed
-define(R_X86_64_PLT32, 4).     % PC-relative 32-bit PLT
-define(R_X86_64_32, 10).       % Direct 32-bit zero-extend
-define(R_X86_64_32S, 11).      % Direct 32-bit sign-extend

-define(ELF_HEADER_SIZE, 64).
-define(SHDR_SIZE, 64).
-define(SYM_SIZE, 24).
-define(RELA_SIZE, 24).

%% ============================================================================
%% Types
%% ============================================================================

-type elf_info() :: #{
    header => elf_header(),
    sections => [section()],
    symbols => [symbol()],
    relocations => #{binary() => [relocation()]}, % section name -> relocations
    strings => #{non_neg_integer() => binary()}    % offset -> string
}.

-type elf_header() :: #{
    class => 64,
    endian => little,
    type => rel,
    machine => x86_64,
    entry => non_neg_integer(),
    shoff => non_neg_integer(),
    shnum => non_neg_integer(),
    shstrndx => non_neg_integer()
}.

-type section() :: #{
    name => binary(),
    type => atom(),
    flags => non_neg_integer(),
    addr => non_neg_integer(),
    offset => non_neg_integer(),
    size => non_neg_integer(),
    link => non_neg_integer(),
    info => non_neg_integer(),
    addralign => non_neg_integer(),
    entsize => non_neg_integer(),
    data => binary()
}.

-type symbol() :: #{
    name => binary(),
    bind => atom(),
    type => atom(),
    shndx => non_neg_integer(),
    value => non_neg_integer(),
    size => non_neg_integer(),
    resolved_addr => non_neg_integer() | undefined
}.

-type relocation() :: #{
    offset => non_neg_integer(),
    type => atom(),
    symbol => non_neg_integer(),
    addend => integer()
}.

-type loaded_module() :: #{
    code => binary(),
    data => binary(),
    init_addr => non_neg_integer(),
    exported_symbols => #{binary() => non_neg_integer()}
}.

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Parse an ELF binary into structured info.
-spec parse(binary()) -> {ok, elf_info()} | {error, term()}.
parse(Binary) ->
    try
        {ok, Header} = parse_elf_header(Binary),
        {ok, Sections, _ShStrTab} = parse_section_headers(Binary, Header),
        {ok, Symbols, SymStrTab} = parse_symbols(Binary, Sections),
        {ok, Relocations} = parse_relocations(Binary, Sections, SymStrTab),

        ElfInfo = #{
            header => Header,
            sections => Sections,
            symbols => Symbols,
            relocations => Relocations,
            strings => SymStrTab
        },
        {ok, ElfInfo}
    catch
        error:Reason:Stack ->
            {error, {parse_failed, Reason, Stack}}
    end.

%% @doc Resolve undefined symbols against a symbol table.
-spec resolve_symbols(elf_info(), #{binary() => non_neg_integer()}) ->
    {ok, elf_info()} | {error, [binary()]}.
resolve_symbols(#{symbols := Symbols} = ElfInfo, SymbolTable) ->
    {Resolved, Unresolved} = lists:foldl(
        fun(Sym, {AccResolved, AccUnresolved}) ->
            case resolve_symbol(Sym, SymbolTable) of
                {ok, ResolvedSym} ->
                    {[ResolvedSym | AccResolved], AccUnresolved};
                {error, Name} ->
                    {[Sym | AccResolved], [Name | AccUnresolved]}
            end
        end,
        {[], []},
        Symbols
    ),

    case Unresolved of
        [] ->
            {ok, ElfInfo#{symbols := lists:reverse(Resolved)}};
        _ ->
            {error, lists:reverse(Unresolved)}
    end.

%% @doc Apply relocations to sections at a given base address.
-spec apply_relocations(elf_info(), non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
apply_relocations(#{sections := Sections, symbols := Symbols, relocations := Relocs} = _ElfInfo, BaseAddr) ->
    try
        %% Build section address map by section index
        %% We need to assign addresses to each allocated section
        {SectionAddrs, _NextAddr} = lists:foldl(
            fun(Section, {AddrMap, Addr}) ->
                case maps:get(flags, Section, 0) band ?SHF_ALLOC of
                    0 ->
                        %% Not allocated
                        {AddrMap, Addr};
                    _ ->
                        %% Allocate this section
                        Size = maps:get(size, Section, 0),
                        {AddrMap#{Section => Addr}, align(Addr + Size, 16)}
                end
            end,
            {#{}, BaseAddr},
            Sections
        ),

        %% Apply relocations to each section
        RelocatedSections = lists:map(
            fun(Section) ->
                _SectionAddr = maps:get(Section, SectionAddrs, 0),
                apply_section_relocations(Section, Relocs, Symbols, Sections, SectionAddrs)
            end,
            Sections
        ),

        %% Concatenate all allocated sections WITH ALIGNMENT PADDING
        {Code, _FinalOffset} = lists:foldl(
            fun(S, {Acc, CurrentOffset}) ->
                case maps:get(flags, S, 0) band ?SHF_ALLOC of
                    0 ->
                        %% Not allocated, skip
                        {Acc, CurrentOffset};
                    _ ->
                        %% Get section data and target address
                        SectionData = maps:get(data, S, <<>>),
                        SectionSize = byte_size(SectionData),
                        TargetAddr = maps:get(S, SectionAddrs, 0),

                        %% Calculate padding needed to match aligned virtual address
                        Padding = max(0, (TargetAddr - BaseAddr) - CurrentOffset),
                        PaddingBinary = <<0:(Padding * 8)>>,

                        %% Append padding + section data
                        NewAcc = <<Acc/binary, PaddingBinary/binary, SectionData/binary>>,
                        NewOffset = CurrentOffset + Padding + SectionSize,
                        {NewAcc, NewOffset}
                end
            end,
            {<<>>, 0},
            RelocatedSections
        ),

        {ok, Code}
    catch
        error:Reason:Stack ->
            {error, {relocation_failed, Reason, Stack}}
    end.

%% @doc High-level load: parse, resolve, relocate.
-spec load(file:filename(), #{binary() => non_neg_integer()}) ->
    {ok, loaded_module()} | {error, term()}.
load(FilePath, SymbolTable) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            case parse(Binary) of
                {ok, ElfInfo} ->
                    case resolve_symbols(ElfInfo, SymbolTable) of
                        {ok, ResolvedElf} ->
                            BaseAddr = 16#1000000, % arbitrary base
                            case apply_relocations(ResolvedElf, BaseAddr) of
                                {ok, Code} ->
                                    %% Extract init function and exports
                                    InitAddr = find_init_addr(ResolvedElf, BaseAddr),
                                    Exports = extract_exports(ResolvedElf, BaseAddr),
                                    Module = #{
                                        code => Code,
                                        data => <<>>,
                                        init_addr => InitAddr,
                                        exported_symbols => Exports
                                    },
                                    {ok, Module};
                                {error, _} = Err ->
                                    Err
                            end;
                        {error, Unresolved} ->
                            {error, {unresolved_symbols, Unresolved}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% ============================================================================
%% ELF Header Parsing
%% ============================================================================

parse_elf_header(<<Magic:4/binary, Class, Endian, Version, _ABI:8, _Pad:64,
                   Type:16/little, Machine:16/little, EVersion:32/little,
                   Entry:64/little, _PhOff:64/little, ShOff:64/little,
                   _Flags:32/little, _EhSize:16/little, _PhEntSize:16/little,
                   _PhNum:16/little, _ShEntSize:16/little, ShNum:16/little,
                   ShStrNdx:16/little, _Rest/binary>>) ->

    %% Validate magic
    case Magic of
        ?ELFMAG -> ok;
        _ -> error({invalid_elf_magic, Magic})
    end,

    %% Validate class (64-bit only)
    case Class of
        ?ELFCLASS64 -> ok;
        _ -> error({unsupported_class, Class})
    end,

    %% Validate endianness (little endian only)
    case Endian of
        ?ELFDATA2LSB -> ok;
        _ -> error({unsupported_endian, Endian})
    end,

    %% Validate version
    case {Version, EVersion} of
        {?EV_CURRENT, ?EV_CURRENT} -> ok;
        _ -> error({invalid_version, Version, EVersion})
    end,

    %% Parse type
    ElfType = case Type of
        ?ET_REL -> rel;
        _ -> error({unsupported_type, Type})
    end,

    %% Parse machine
    ElfMachine = case Machine of
        ?EM_X86_64 -> x86_64;
        _ -> error({unsupported_machine, Machine})
    end,

    Header = #{
        class => 64,
        endian => little,
        type => ElfType,
        machine => ElfMachine,
        entry => Entry,
        shoff => ShOff,
        shnum => ShNum,
        shstrndx => ShStrNdx
    },

    {ok, Header};
parse_elf_header(_) ->
    error(truncated_elf_header).

%% ============================================================================
%% Section Header Parsing
%% ============================================================================

parse_section_headers(Binary, #{shoff := ShOff, shnum := ShNum, shstrndx := ShStrNdx}) ->
    %% Read all section headers
    SectionHeaders = [
        parse_section_header(Binary, ShOff + (I * ?SHDR_SIZE))
        || I <- lists:seq(0, ShNum - 1)
    ],

    %% Get the .shstrtab section
    ShStrTabHdr = lists:nth(ShStrNdx + 1, SectionHeaders),
    ShStrTabData = extract_section_data(Binary, ShStrTabHdr),

    %% Resolve section names
    Sections = [
        resolve_section_name(Hdr, ShStrTabData, Binary)
        || Hdr <- SectionHeaders
    ],

    {ok, Sections, ShStrTabData}.

parse_section_header(Binary, Offset) ->
    <<_:Offset/binary,
      Name:32/little, Type:32/little, Flags:64/little,
      Addr:64/little, SecOffset:64/little, Size:64/little,
      Link:32/little, Info:32/little, AddrAlign:64/little,
      EntSize:64/little, _/binary>> = Binary,

    #{
        name_offset => Name,
        type => sh_type_to_atom(Type),
        flags => Flags,
        addr => Addr,
        offset => SecOffset,
        size => Size,
        link => Link,
        info => Info,
        addralign => AddrAlign,
        entsize => EntSize
    }.

resolve_section_name(#{name_offset := NameOff} = Hdr, ShStrTab, Binary) ->
    Name = extract_string(ShStrTab, NameOff),
    Data = extract_section_data(Binary, Hdr),
    Hdr#{name => Name, data => Data}.

extract_section_data(_Binary, #{type := nobits}) ->
    <<>>;
extract_section_data(Binary, #{offset := Offset, size := Size}) ->
    <<_:Offset/binary, Data:Size/binary, _/binary>> = Binary,
    Data.

%% ============================================================================
%% Symbol Table Parsing
%% ============================================================================

parse_symbols(_Binary, Sections) ->
    %% Find .symtab section
    case lists:search(fun(#{type := Type}) -> Type =:= symtab end, Sections) of
        {value, SymTabSec} ->
            %% Find .strtab section (linked from .symtab)
            #{link := StrTabIdx, data := SymTabData, entsize := EntSize} = SymTabSec,
            StrTabSec = lists:nth(StrTabIdx + 1, Sections),
            #{data := StrTabData} = StrTabSec,

            %% Parse symbol entries
            NumSyms = byte_size(SymTabData) div EntSize,
            Symbols = [
                parse_symbol(SymTabData, I * ?SYM_SIZE, StrTabData)
                || I <- lists:seq(0, NumSyms - 1)
            ],

            %% Build string table map
            StrTab = build_string_table(StrTabData),

            {ok, Symbols, StrTab};
        false ->
            {ok, [], #{}}
    end.

parse_symbol(SymTabData, Offset, StrTabData) ->
    <<_:Offset/binary,
      Name:32/little, Info:8, _Other:8, Shndx:16/little,
      Value:64/little, Size:64/little, _/binary>> = SymTabData,

    Bind = sym_bind(Info bsr 4),
    Type = sym_type(Info band 16#F),
    NameStr = extract_string(StrTabData, Name),

    #{
        name => NameStr,
        bind => Bind,
        type => Type,
        shndx => Shndx,
        value => Value,
        size => Size,
        resolved_addr => undefined
    }.

%% ============================================================================
%% Relocation Parsing
%% ============================================================================

parse_relocations(_Binary, Sections, _StrTab) ->
    %% Find all .rela.* sections
    RelaSections = [S || S <- Sections, is_rela_section(maps:get(name, S, <<>>))],

    %% Parse each .rela section
    RelocMap = lists:foldl(
        fun(#{name := _Name, data := Data, entsize := EntSize, info := TargetIdx}, Acc) ->
            TargetSec = lists:nth(TargetIdx + 1, Sections),
            TargetName = maps:get(name, TargetSec),

            NumRelas = byte_size(Data) div EntSize,
            Relocs = [
                parse_rela(Data, I * ?RELA_SIZE)
                || I <- lists:seq(0, NumRelas - 1)
            ],

            Acc#{TargetName => Relocs}
        end,
        #{},
        RelaSections
    ),

    {ok, RelocMap}.

parse_rela(Data, Offset) ->
    <<_:Offset/binary,
      RelaOffset:64/little, Info:64/little, Addend:64/signed-little,
      _/binary>> = Data,

    Sym = Info bsr 32,
    Type = rela_type(Info band 16#FFFFFFFF),

    #{
        offset => RelaOffset,
        type => Type,
        symbol => Sym,
        addend => Addend
    }.

is_rela_section(<<".rela.", _/binary>>) -> true;
is_rela_section(_) -> false.

%% ============================================================================
%% Symbol Resolution
%% ============================================================================

resolve_symbol(#{shndx := ?SHN_UNDEF, name := <<>>} = Sym, _SymbolTable) ->
    %% Empty symbol name (null symbol) - skip
    {ok, Sym};
resolve_symbol(#{shndx := ?SHN_UNDEF, name := Name} = Sym, SymbolTable) ->
    case maps:find(Name, SymbolTable) of
        {ok, Addr} ->
            {ok, Sym#{resolved_addr := Addr}};
        error ->
            {error, Name}
    end;
resolve_symbol(Sym, _SymbolTable) ->
    %% Already defined in this object
    {ok, Sym}.

%% ============================================================================
%% Relocation Application
%% ============================================================================

apply_section_relocations(Section, Relocs, Symbols, AllSections, SectionAddrs) ->
    Name = maps:get(name, Section),
    case maps:find(Name, Relocs) of
        {ok, RelocList} ->
            Data = maps:get(data, Section),
            SectionAddr = maps:get(Section, SectionAddrs, 0),

            RelocatedData = lists:foldl(
                fun(Reloc, Acc) ->
                    apply_relocation(Reloc, Acc, Symbols, AllSections, SectionAddrs, SectionAddr)
                end,
                Data,
                RelocList
            ),

            Section#{data := RelocatedData};
        error ->
            Section
    end.

apply_relocation(#{offset := Offset, type := Type, symbol := SymIdx, addend := Addend},
                 Data, Symbols, AllSections, SectionAddrs, CurrentSectionAddr) ->
    %% Get symbol address
    Sym = lists:nth(SymIdx + 1, Symbols),
    SymAddr = calculate_symbol_address(Sym, AllSections, SectionAddrs),

    %% Calculate relocation value
    P = CurrentSectionAddr + Offset,  % Place (address being patched)
    S = SymAddr,                      % Symbol address
    A = Addend,                       % Addend

    Value = case Type of
        r_x86_64_64 ->
            %% S + A
            S + A;
        r_x86_64_pc32 ->
            %% S + A - P
            (S + A - P) band 16#FFFFFFFF;
        r_x86_64_plt32 ->
            %% L + A - P (for now, treat like PC32)
            (S + A - P) band 16#FFFFFFFF;
        r_x86_64_32 ->
            %% S + A (zero-extend)
            (S + A) band 16#FFFFFFFF;
        r_x86_64_32s ->
            %% S + A (sign-extend)
            (S + A) band 16#FFFFFFFF;
        r_x86_64_none ->
            0
    end,

    %% Patch the data
    patch_data(Data, Offset, Value, Type).

patch_data(Data, _Offset, _Value, 0) ->
    %% R_X86_64_NONE - no patching needed
    Data;
patch_data(Data, Offset, Value, Type) ->
    Width = reloc_width(Type),
    WidthBytes = Width div 8,
    DataSize = byte_size(Data),

    %% SECURITY: Validate relocation offset to prevent OOM
    %% Maximum reasonable section size: 256MB
    MaxSectionSize = 256 * 1024 * 1024,
    case Offset + WidthBytes of
        N when N > MaxSectionSize ->
            %% Reject relocations that would require excessive padding
            error({relocation_offset_too_large, Offset, DataSize});
        N when N > DataSize ->
            %% Pad data if needed (validated size)
            PadSize = N - DataSize,
            PaddedData = <<Data/binary, 0:(PadSize * 8)>>,
            patch_data(PaddedData, Offset, Value, Type);
        _ ->
            <<Before:Offset/binary, _:WidthBytes/binary, After/binary>> = Data,
            Patch = case Width of
                32 -> <<Value:32/little>>;
                64 -> <<Value:64/little>>
            end,
            <<Before/binary, Patch/binary, After/binary>>
    end.

reloc_width(r_x86_64_64) -> 64;
reloc_width(r_x86_64_pc32) -> 32;
reloc_width(r_x86_64_plt32) -> 32;
reloc_width(r_x86_64_32) -> 32;
reloc_width(r_x86_64_32s) -> 32;
reloc_width(r_x86_64_none) -> 0.

%% Calculate effective symbol address for relocation
calculate_symbol_address(#{resolved_addr := ResolvedAddr, value := Value}, _AllSections, _SectionAddrs)
  when ResolvedAddr =/= undefined ->
    %% External symbol - use resolved address
    ResolvedAddr + Value;
calculate_symbol_address(#{shndx := ?SHN_UNDEF}, _AllSections, _SectionAddrs) ->
    %% Unresolved external - should have been caught earlier
    0;
calculate_symbol_address(#{shndx := ?SHN_ABS, value := Value}, _AllSections, _SectionAddrs) ->
    %% Absolute symbol
    Value;
calculate_symbol_address(#{shndx := Shndx, value := Value}, AllSections, SectionAddrs) when Shndx > 0 ->
    %% Section-relative symbol - look up the section by index
    TargetSection = lists:nth(Shndx + 1, AllSections),
    SectionAddr = maps:get(TargetSection, SectionAddrs, 0),
    SectionAddr + Value;
calculate_symbol_address(_Sym, _AllSections, _SectionAddrs) ->
    0.

%% ============================================================================
%% Helper Functions
%% ============================================================================

find_init_addr(#{symbols := Symbols}, BaseAddr) ->
    case lists:search(
        fun(#{name := N}) -> N =:= <<"init_module">> orelse N =:= <<"__init">> end,
        Symbols
    ) of
        {value, #{value := Val, resolved_addr := Addr}} ->
            coalesce(Addr, BaseAddr) + Val;
        false ->
            BaseAddr
    end.

extract_exports(#{symbols := Symbols}, BaseAddr) ->
    lists:foldl(
        fun(#{name := Name, bind := global, value := Val, resolved_addr := Addr}, Acc) ->
            Acc#{Name => coalesce(Addr, BaseAddr) + Val};
           (_, Acc) ->
            Acc
        end,
        #{},
        Symbols
    ).

coalesce(undefined, Default) -> Default;
coalesce(Value, _Default) -> Value.

extract_string(StrTab, Offset) ->
    <<_:Offset/binary, Rest/binary>> = StrTab,
    extract_cstring(Rest, <<>>).

extract_cstring(<<0, _/binary>>, Acc) ->
    Acc;
extract_cstring(<<C, Rest/binary>>, Acc) ->
    extract_cstring(Rest, <<Acc/binary, C>>);
extract_cstring(<<>>, Acc) ->
    Acc.

build_string_table(StrTab) ->
    build_string_table(StrTab, 0, #{}).

build_string_table(<<>>, _Offset, Acc) ->
    Acc;
build_string_table(<<0, Rest/binary>>, Offset, Acc) ->
    build_string_table(Rest, Offset + 1, Acc);
build_string_table(Data, Offset, Acc) ->
    Str = extract_cstring(Data, <<>>),
    Len = byte_size(Str) + 1,
    <<_:Len/binary, Rest/binary>> = Data,
    build_string_table(Rest, Offset + Len, Acc#{Offset => Str}).

%% ============================================================================
%% Alignment
%% ============================================================================

align(Value, Boundary) ->
    case Value rem Boundary of
        0 -> Value;
        R -> Value + (Boundary - R)
    end.

%% ============================================================================
%% Constant Mapping
%% ============================================================================

sh_type_to_atom(?SHT_NULL) -> null;
sh_type_to_atom(?SHT_PROGBITS) -> progbits;
sh_type_to_atom(?SHT_SYMTAB) -> symtab;
sh_type_to_atom(?SHT_STRTAB) -> strtab;
sh_type_to_atom(?SHT_RELA) -> rela;
sh_type_to_atom(?SHT_NOBITS) -> nobits;
sh_type_to_atom(?SHT_REL) -> rel;
sh_type_to_atom(N) -> {unknown, N}.

sym_bind(?STB_LOCAL) -> local;
sym_bind(?STB_GLOBAL) -> global;
sym_bind(?STB_WEAK) -> weak;
sym_bind(N) -> {unknown, N}.

sym_type(?STT_NOTYPE) -> notype;
sym_type(?STT_OBJECT) -> object;
sym_type(?STT_FUNC) -> func;
sym_type(?STT_SECTION) -> section;
sym_type(?STT_FILE) -> file;
sym_type(N) -> {unknown, N}.

rela_type(?R_X86_64_NONE) -> r_x86_64_none;
rela_type(?R_X86_64_64) -> r_x86_64_64;
rela_type(?R_X86_64_PC32) -> r_x86_64_pc32;
rela_type(?R_X86_64_PLT32) -> r_x86_64_plt32;
rela_type(?R_X86_64_32) -> r_x86_64_32;
rela_type(?R_X86_64_32S) -> r_x86_64_32s;
rela_type(N) -> {unknown, N}.
