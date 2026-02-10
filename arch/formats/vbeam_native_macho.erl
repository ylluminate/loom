%%% @doc Mach-O 64-bit executable emitter for macOS.
%%% Produces a dynamically-linked MH_EXECUTE Mach-O binary.
%%% macOS ARM64 requires dynamic linking against libSystem.
%%% @end
-module(vbeam_native_macho).

-export([emit/4]).

%% ============================================================================
%% Mach-O Constants
%% ============================================================================

-define(MH_MAGIC_64, 16#FEEDFACF).
-define(CPU_TYPE_X86_64, 16#01000007).
-define(CPU_TYPE_ARM64, 16#0100000C).
-define(CPU_SUBTYPE_ALL, 3).
-define(CPU_SUBTYPE_ARM64_ALL, 0).
-define(MH_EXECUTE, 2).
-define(MH_NOUNDEFS, 16#01).
-define(MH_DYLDLINK, 16#04).
-define(MH_TWOLEVEL, 16#80).
-define(MH_PIE, 16#200000).
%% MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE
-define(MH_FLAGS, 16#200085).

-define(LC_SEGMENT_64, 16#19).
-define(LC_SYMTAB, 16#02).
-define(LC_LOAD_DYLINKER, 16#0E).
-define(LC_LOAD_DYLIB, 16#0C).
-define(LC_MAIN, 16#80000028).
-define(LC_BUILD_VERSION, 16#32).
-define(LC_DYLD_INFO_ONLY, 16#80000022).
-define(LC_DYLD_CHAINED_FIXUPS, 16#80000034).
-define(LC_DYLD_EXPORTS_TRIE, 16#80000033).

-define(S_REGULAR, 0).
-define(S_ATTR_PURE_INSTRUCTIONS, 16#80000000).
-define(S_ATTR_SOME_INSTRUCTIONS, 16#00000400).

-define(VM_PROT_READ, 1).
-define(VM_PROT_WRITE, 2).
-define(VM_PROT_EXECUTE, 4).

-define(MACHO_HEADER_SIZE, 32).
-define(TEXT_SEG_BASE, 16#100000000).
-define(LC_SEGMENT_64_BASE_SIZE, 72).
-define(SECTION_64_SIZE, 80).

%% ============================================================================
%% Public API
%% ============================================================================

-spec emit(binary(), binary(), non_neg_integer(), x86_64 | arm64) -> binary().
emit(Code, Data, EntryOffset, Arch) ->
    PageSize = page_size(Arch),
    {CpuType, CpuSubtype} = arch_to_cpu(Arch),
    CodeSize = byte_size(Code),
    DataSize = byte_size(Data),

    %% Calculate load command sizes
    PageZeroLCSize = ?LC_SEGMENT_64_BASE_SIZE,
    TextLCSize = ?LC_SEGMENT_64_BASE_SIZE + ?SECTION_64_SIZE,
    DataLCSize = ?LC_SEGMENT_64_BASE_SIZE + ?SECTION_64_SIZE,
    LinkeditLCSize = ?LC_SEGMENT_64_BASE_SIZE,
    DylinkerLCSize = lc_load_dylinker_size(),
    MainLCSize = 24,
    DylibLCSize = lc_load_dylib_size(),
    BuildVersionLCSize = 24,
    ChainedFixupsLCSize = 16,
    ExportsTrieLCSize = 16,
    SymtabLCSize = 24,

    NumCmds = 11,
    SizeOfCmds = PageZeroLCSize + TextLCSize + DataLCSize +
                 LinkeditLCSize + DylinkerLCSize + MainLCSize +
                 DylibLCSize + BuildVersionLCSize +
                 ChainedFixupsLCSize + ExportsTrieLCSize +
                 SymtabLCSize,
    HeaderAndCmds = ?MACHO_HEADER_SIZE + SizeOfCmds,

    %% __TEXT segment starts at file offset 0 and includes header + commands.
    TextSectionOff = page_align(HeaderAndCmds, PageSize),
    TextSegFileSize = page_align(TextSectionOff + CodeSize, PageSize),
    TextSegVMSize = TextSegFileSize,
    TextSegVAddr = ?TEXT_SEG_BASE,
    TextSectionVAddr = TextSegVAddr + TextSectionOff,

    %% __DATA segment follows __TEXT
    DataSegFileOff = TextSegFileSize,
    DataSegFileSize = case DataSize of
        0 -> PageSize;
        _ -> page_align(DataSize, PageSize)
    end,
    DataSegVMSize = DataSegFileSize,
    DataSegVAddr = TextSegVAddr + TextSegFileSize,
    DataSectionVAddr = DataSegVAddr,

    %% __LINKEDIT follows __DATA
    LinkeditFileOff = DataSegFileOff + DataSegFileSize,
    LinkeditFileSize = PageSize,
    LinkeditVMSize = LinkeditFileSize,
    LinkeditVAddr = DataSegVAddr + DataSegVMSize,

    %% __LINKEDIT layout:
    %% [chained_fixups_header (28 bytes)] [starts_in_image (8 bytes)]
    %% [exports_trie (2 bytes min)] [symtab string table (4 bytes)]
    ChainedFixupsOff = LinkeditFileOff,
    ChainedFixupsSize = 36,  %% 28 byte header + 8 byte starts_in_image (1 segment, offset 0)
    ExportsTrieOff = ChainedFixupsOff + ChainedFixupsSize,
    ExportsTrieSize = 2,  %% minimal trie: just a terminal node
    SymtabStrOff = align_up(ExportsTrieOff + ExportsTrieSize, 4),
    SymtabOff = SymtabStrOff,  %% no symbols, just string table

    %% Entry offset from start of __TEXT segment
    true = EntryOffset < byte_size(Code),  %% Validate entry offset
    EntryOff = TextSectionOff + EntryOffset,

    %% Build Mach-O header
    Header = build_header(CpuType, CpuSubtype, NumCmds, SizeOfCmds),

    %% Build load commands (order matters for macOS loader)
    LC1 = build_pagezero_segment(),
    LC2 = build_text_segment(TextSegVAddr, TextSegVMSize, TextSegFileSize,
                             TextSectionVAddr, CodeSize, TextSectionOff),
    LC3 = build_data_segment(DataSegVAddr, DataSegVMSize,
                             DataSegFileOff, DataSegFileSize,
                             DataSectionVAddr, DataSize, DataSegFileOff),
    LC4 = build_linkedit_segment(LinkeditVAddr, LinkeditVMSize,
                                  LinkeditFileOff, LinkeditFileSize),
    LC5 = build_lc_load_dylinker(),
    LC6 = build_lc_main(EntryOff),
    LC7 = build_lc_load_dylib(),
    LC8 = build_lc_build_version(),
    LC9 = build_lc_chained_fixups(ChainedFixupsOff, ChainedFixupsSize),
    LC10 = build_lc_exports_trie(ExportsTrieOff, ExportsTrieSize),
    LC11 = build_lc_symtab(SymtabOff),

    HeaderAndCommands = <<Header/binary, LC1/binary, LC2/binary, LC3/binary,
                          LC4/binary, LC5/binary, LC6/binary, LC7/binary,
                          LC8/binary, LC9/binary, LC10/binary, LC11/binary>>,

    %% Pad to text section offset
    TextPad = padding(byte_size(HeaderAndCommands), TextSectionOff),

    %% Pad after code to fill text segment
    AfterCode = TextSectionOff + CodeSize,
    TextSegPad = padding(AfterCode, TextSegFileSize),

    %% Pad after data to fill data segment
    DataSegPad = padding(DataSize, DataSegFileSize),

    %% __LINKEDIT content
    %% Chained fixups header (28 bytes):
    %%   fixups_version=0, starts_offset=28, imports_offset=36,
    %%   symbols_offset=36, imports_count=0, imports_format=1, symbols_format=0
    ChainedFixupsHdr = <<0:32/little, 28:32/little, 36:32/little,
                          36:32/little, 0:32/little, 1:32/little, 0:32/little>>,
    %% Chained starts in image (8 bytes): seg_count=0 (no fixups in any segment)
    ChainedStarts = <<0:32/little, 0:32/little>>,
    %% Exports trie: minimal (empty trie = just 0x00 0x00)
    ExportsTrie = <<0:8, 0:8>>,
    %% Padding to align symtab
    LinkeditData = <<ChainedFixupsHdr/binary, ChainedStarts/binary, ExportsTrie/binary>>,
    LinkeditPad = padding(byte_size(LinkeditData), SymtabStrOff - LinkeditFileOff),
    %% String table: null byte + pad to 4
    SymtabStr = <<0:8, 0:8, 0:8, 0:8>>,
    LinkeditUsed = <<LinkeditData/binary, LinkeditPad/binary, SymtabStr/binary>>,
    RemainingLinkedit = LinkeditFileSize - byte_size(LinkeditUsed),
    LinkeditContent = <<LinkeditUsed/binary, 0:(RemainingLinkedit * 8)>>,

    <<HeaderAndCommands/binary,
      TextPad/binary,
      Code/binary,
      TextSegPad/binary,
      Data/binary,
      DataSegPad/binary,
      LinkeditContent/binary>>.

%% ============================================================================
%% Mach-O Header
%% ============================================================================

build_header(CpuType, CpuSubtype, NumCmds, SizeOfCmds) ->
    <<?MH_MAGIC_64:32/little,
      CpuType:32/little,
      CpuSubtype:32/little,
      ?MH_EXECUTE:32/little,
      NumCmds:32/little,
      SizeOfCmds:32/little,
      ?MH_FLAGS:32/little,
      0:32/little>>.

%% ============================================================================
%% LC_SEGMENT_64 - __PAGEZERO
%% ============================================================================

build_pagezero_segment() ->
    CmdSize = ?LC_SEGMENT_64_BASE_SIZE,
    SegName = pad_name(<<"__PAGEZERO">>, 16),
    <<?LC_SEGMENT_64:32/little, CmdSize:32/little, SegName/binary,
      0:64/little, ?TEXT_SEG_BASE:64/little, 0:64/little, 0:64/little,
      0:32/little, 0:32/little, 0:32/little, 0:32/little>>.

%% ============================================================================
%% LC_SEGMENT_64 - __TEXT
%% ============================================================================

build_text_segment(SegVAddr, VMSize, FileSize,
                   SectionVAddr, SectionSize, SectionFileOff) ->
    CmdSize = ?LC_SEGMENT_64_BASE_SIZE + ?SECTION_64_SIZE,
    SegName = pad_name(<<"__TEXT">>, 16),
    MaxProt = ?VM_PROT_READ bor ?VM_PROT_EXECUTE,
    InitProt = MaxProt,
    SegCmd = <<(?LC_SEGMENT_64):32/little, CmdSize:32/little, SegName/binary,
               SegVAddr:64/little, VMSize:64/little,
               0:64/little, FileSize:64/little,
               MaxProt:32/little, InitProt:32/little,
               1:32/little, 0:32/little>>,
    SectName = pad_name(<<"__text">>, 16),
    SectSegName = pad_name(<<"__TEXT">>, 16),
    SectionFlags = ?S_ATTR_PURE_INSTRUCTIONS bor ?S_ATTR_SOME_INSTRUCTIONS,
    Section = <<SectName/binary, SectSegName/binary,
                SectionVAddr:64/little, SectionSize:64/little,
                SectionFileOff:32/little, 4:32/little,
                0:32/little, 0:32/little,
                SectionFlags:32/little,
                0:32/little, 0:32/little, 0:32/little>>,
    <<SegCmd/binary, Section/binary>>.

%% ============================================================================
%% LC_SEGMENT_64 - __DATA
%% ============================================================================

build_data_segment(SegVAddr, VMSize, SegFileOff, SegFileSize,
                   SectionVAddr, SectionSize, SectionFileOff) ->
    CmdSize = ?LC_SEGMENT_64_BASE_SIZE + ?SECTION_64_SIZE,
    SegName = pad_name(<<"__DATA">>, 16),
    MaxProt = ?VM_PROT_READ bor ?VM_PROT_WRITE,
    InitProt = MaxProt,
    SegCmd = <<(?LC_SEGMENT_64):32/little, CmdSize:32/little, SegName/binary,
               SegVAddr:64/little, VMSize:64/little,
               SegFileOff:64/little, SegFileSize:64/little,
               MaxProt:32/little, InitProt:32/little,
               1:32/little, 0:32/little>>,
    SectName = pad_name(<<"__data">>, 16),
    SectSegName = pad_name(<<"__DATA">>, 16),
    Section = <<SectName/binary, SectSegName/binary,
                SectionVAddr:64/little, SectionSize:64/little,
                SectionFileOff:32/little, 3:32/little,
                0:32/little, 0:32/little,
                (?S_REGULAR):32/little,
                0:32/little, 0:32/little, 0:32/little>>,
    <<SegCmd/binary, Section/binary>>.

%% ============================================================================
%% LC_SEGMENT_64 - __LINKEDIT
%% ============================================================================

build_linkedit_segment(VAddr, VMSize, FileOff, FileSize) ->
    CmdSize = ?LC_SEGMENT_64_BASE_SIZE,
    SegName = pad_name(<<"__LINKEDIT">>, 16),
    MaxProt = ?VM_PROT_READ,
    InitProt = MaxProt,
    <<?LC_SEGMENT_64:32/little, CmdSize:32/little, SegName/binary,
      VAddr:64/little, VMSize:64/little,
      FileOff:64/little, FileSize:64/little,
      MaxProt:32/little, InitProt:32/little,
      0:32/little, 0:32/little>>.

%% ============================================================================
%% LC_LOAD_DYLINKER — tells kernel to invoke dyld
%% ============================================================================

-define(DYLINKER_PATH, <<"/usr/lib/dyld">>).

lc_load_dylinker_size() ->
    %% cmd(4) + cmdsize(4) + str_offset(4) + string + null + padding to 8
    RawSize = 12 + byte_size(?DYLINKER_PATH) + 1,
    align_up(RawSize, 8).

build_lc_load_dylinker() ->
    CmdSize = lc_load_dylinker_size(),
    StrOffset = 12,  %% offset from start of load command to string
    Path = ?DYLINKER_PATH,
    PadSize = CmdSize - 12 - byte_size(Path) - 1,
    <<(?LC_LOAD_DYLINKER):32/little, CmdSize:32/little, StrOffset:32/little,
      Path/binary, 0:8, 0:(PadSize * 8)>>.

%% ============================================================================
%% LC_MAIN — entry offset (dyld-based, replaces LC_UNIXTHREAD)
%% ============================================================================

build_lc_main(EntryOff) ->
    <<(?LC_MAIN):32/little, 24:32/little,
      EntryOff:64/little, 0:64/little>>.

%% ============================================================================
%% LC_LOAD_DYLIB — reference to libSystem.B.dylib (mandatory on macOS)
%% ============================================================================

-define(LIBSYSTEM_PATH, <<"/usr/lib/libSystem.B.dylib">>).

lc_load_dylib_size() ->
    %% cmd(4) + cmdsize(4) + str_offset(4) + timestamp(4) +
    %% current_version(4) + compat_version(4) + string + null + padding to 8
    RawSize = 24 + byte_size(?LIBSYSTEM_PATH) + 1,
    align_up(RawSize, 8).

build_lc_load_dylib() ->
    CmdSize = lc_load_dylib_size(),
    StrOffset = 24,  %% offset from start of load command to string
    Path = ?LIBSYSTEM_PATH,
    PadSize = CmdSize - 24 - byte_size(Path) - 1,
    Timestamp = 2,  %% standard timestamp for libSystem
    CurrentVersion = 16#04B50100,  %% 1292.1.0 (recent enough)
    CompatVersion = 16#00010000,   %% 1.0.0
    <<(?LC_LOAD_DYLIB):32/little, CmdSize:32/little, StrOffset:32/little,
      Timestamp:32/little, CurrentVersion:32/little, CompatVersion:32/little,
      Path/binary, 0:8, 0:(PadSize * 8)>>.

%% ============================================================================
%% LC_BUILD_VERSION (required for ARM64 macOS executables)
%% ============================================================================

build_lc_build_version() ->
    Platform = 1,       %% PLATFORM_MACOS
    MinOS = 16#000B0000,%% 11.0.0 (minimum ARM64 macOS)
    SDK = 16#000B0000,
    <<(?LC_BUILD_VERSION):32/little, 24:32/little,
      Platform:32/little, MinOS:32/little, SDK:32/little, 0:32/little>>.

%% ============================================================================
%% LC_DYLD_CHAINED_FIXUPS — points to chained fixups in __LINKEDIT
%% ============================================================================

build_lc_chained_fixups(DataOff, DataSize) ->
    <<(?LC_DYLD_CHAINED_FIXUPS):32/little, 16:32/little,
      DataOff:32/little, DataSize:32/little>>.

%% ============================================================================
%% LC_DYLD_EXPORTS_TRIE — points to exports trie in __LINKEDIT
%% ============================================================================

build_lc_exports_trie(DataOff, DataSize) ->
    <<(?LC_DYLD_EXPORTS_TRIE):32/little, 16:32/little,
      DataOff:32/little, DataSize:32/little>>.

%% ============================================================================
%% LC_SYMTAB (minimal/empty)
%% ============================================================================

build_lc_symtab(SymtabOff) ->
    <<(?LC_SYMTAB):32/little, 24:32/little,
      SymtabOff:32/little, 0:32/little,
      SymtabOff:32/little, 4:32/little>>.

%% ============================================================================
%% Architecture Mapping
%% ============================================================================

arch_to_cpu(x86_64) -> {?CPU_TYPE_X86_64, ?CPU_SUBTYPE_ALL};
arch_to_cpu(arm64)  -> {?CPU_TYPE_ARM64, ?CPU_SUBTYPE_ARM64_ALL}.

page_size(x86_64) -> 16#1000;
page_size(arm64)  -> 16#4000.

%% ============================================================================
%% Utilities
%% ============================================================================

-spec pad_name(binary(), pos_integer()) -> binary().
pad_name(Name, Len) when byte_size(Name) =< Len ->
    PadBytes = Len - byte_size(Name),
    <<Name/binary, 0:(PadBytes * 8)>>.

-spec page_align(non_neg_integer(), pos_integer()) -> non_neg_integer().
page_align(Value, PageSz) ->
    case Value rem PageSz of
        0 -> Value;
        R -> Value + (PageSz - R)
    end.

align_up(Value, Alignment) ->
    case Value rem Alignment of
        0 -> Value;
        R -> Value + (Alignment - R)
    end.

-spec padding(non_neg_integer(), non_neg_integer()) -> binary().
padding(CurrentOff, TargetOff) when TargetOff >= CurrentOff ->
    PadSize = TargetOff - CurrentOff,
    <<0:(PadSize * 8)>>;
padding(CurrentOff, TargetOff) ->
    error({padding_underflow, CurrentOff, TargetOff}).
