%%% @doc PE32+ (64-bit) executable emitter for UEFI and Windows.
%%% Produces a PE/COFF executable suitable for UEFI applications or
%%% Windows console programs. Supports x86_64 and ARM64 machine types.
%%%
%%% PE layout:
%%%   [DOS Header + Stub (64 bytes)]
%%%   [PE Signature "PE\0\0"]
%%%   [COFF File Header (20 bytes)]
%%%   [Optional Header PE32+ (240 bytes)]
%%%   [Section Table (.text, .data, .reloc)]
%%%   [.text section data (code)]
%%%   [.data section data]
%%%   [.reloc section data (minimal/empty)]
%%% @end
-module(vbeam_native_pe).

-compile({no_auto_import,[max/2]}).

-export([emit/4, emit/5]).

%% ============================================================================
%% PE Constants
%% ============================================================================

%% DOS Header magic
-define(DOS_MAGIC, <<"MZ">>).

%% PE Signature
-define(PE_SIG, <<"PE", 0, 0>>).

%% Machine types
-define(IMAGE_FILE_MACHINE_AMD64, 16#8664).
-define(IMAGE_FILE_MACHINE_ARM64, 16#AA64).

%% COFF characteristics
-define(IMAGE_FILE_EXECUTABLE_IMAGE, 16#0002).
-define(IMAGE_FILE_LARGE_ADDRESS_AWARE, 16#0020).

%% PE32+ magic (64-bit)
-define(PE32PLUS_MAGIC, 16#020B).

%% Subsystems
-define(IMAGE_SUBSYSTEM_EFI_APPLICATION, 10).
-define(IMAGE_SUBSYSTEM_WINDOWS_CUI, 3).

%% DLL Characteristics
-define(IMAGE_DLLCHARACTERISTICS_NX_COMPAT, 16#0100).
-define(IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE, 16#0040).

%% Section characteristics
-define(IMAGE_SCN_CNT_CODE, 16#00000020).
-define(IMAGE_SCN_CNT_INITIALIZED_DATA, 16#00000040).
-define(IMAGE_SCN_MEM_DISCARDABLE, 16#02000000).
-define(IMAGE_SCN_MEM_EXECUTE, 16#20000000).
-define(IMAGE_SCN_MEM_READ, 16#40000000).
-define(IMAGE_SCN_MEM_WRITE, 16#80000000).

%% Sizes
-define(DOS_HEADER_SIZE, 64).
-define(PE_SIG_SIZE, 4).
-define(COFF_HEADER_SIZE, 20).
-define(OPTIONAL_HEADER_SIZE, 240).  %% PE32+ with 16 data directories
-define(SECTION_HEADER_SIZE, 40).
-define(FILE_ALIGNMENT, 512).
-define(SECTION_ALIGNMENT, 16#1000).

%% Standard base addresses
-define(IMAGE_BASE_EXE, 16#00400000).
-define(IMAGE_BASE_UEFI, 16#00400000).

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Emit a PE32+ binary, defaulting to UEFI subsystem.
-spec emit(binary(), binary(), non_neg_integer(), x86_64 | arm64) -> binary().
emit(Code, Data, EntryOffset, Arch) ->
    emit(Code, Data, EntryOffset, Arch, efi).

%% @doc Emit a PE32+ binary with specified subsystem.
%% Subsystem: efi | windows
-spec emit(binary(), binary(), non_neg_integer(), x86_64 | arm64,
           efi | windows) -> binary().
emit(Code, Data, EntryOffset, Arch, Subsystem) ->
    Machine = arch_to_machine(Arch),
    SubsysVal = subsystem_value(Subsystem),
    CodeSize = byte_size(Code),
    DataSize = byte_size(Data),

    %% Number of sections: .text, .data, .reloc
    NumSections = 3,

    %% Headers size (DOS + PE sig + COFF + Optional + Section table)
    HeadersRawSize = ?DOS_HEADER_SIZE + ?PE_SIG_SIZE +
                     ?COFF_HEADER_SIZE + ?OPTIONAL_HEADER_SIZE +
                     (NumSections * ?SECTION_HEADER_SIZE),
    HeadersSize = file_align(HeadersRawSize),

    %% .text section
    TextRVA = ?SECTION_ALIGNMENT,
    TextFileOff = HeadersSize,
    TextFileSize = file_align(CodeSize),
    TextVirtualSize = CodeSize,

    %% .data section
    DataRVA = TextRVA + section_align(TextVirtualSize),
    DataFileOff = TextFileOff + TextFileSize,
    DataFileSize = file_align(max(DataSize, 1)),
    DataVirtualSize = max(DataSize, 1),

    %% .reloc section (minimal — just a base relocation block header)
    RelocRVA = DataRVA + section_align(DataVirtualSize),
    RelocFileOff = DataFileOff + DataFileSize,
    RelocData = build_reloc_section(TextRVA),
    RelocFileSize = file_align(byte_size(RelocData)),
    RelocVirtualSize = byte_size(RelocData),

    %% Image size (virtual)
    ImageSize = RelocRVA + section_align(RelocVirtualSize),

    %% Entry point RVA (relative to image base)
    EntryRVA = TextRVA + EntryOffset,

    %% Data directories
    %% Index 5 = Base Relocation Table
    DataDirectories = build_data_directories(RelocRVA, RelocVirtualSize),

    %% Build all the parts
    DosHeader = build_dos_header(),
    CoffHeader = build_coff_header(Machine, NumSections),
    OptionalHeader = build_optional_header(
        EntryRVA, TextRVA, HeadersSize,
        ImageSize, SubsysVal, DataDirectories),
    SectionTable = build_section_table(
        TextRVA, TextVirtualSize, TextFileOff, TextFileSize,
        DataRVA, DataVirtualSize, DataFileOff, DataFileSize,
        RelocRVA, RelocVirtualSize, RelocFileOff, RelocFileSize),

    %% Assemble headers
    Headers = <<DosHeader/binary, ?PE_SIG/binary,
                CoffHeader/binary, OptionalHeader/binary,
                SectionTable/binary>>,

    %% Pad headers to HeadersSize
    HeaderPad = padding(byte_size(Headers), HeadersSize),

    %% Pad code to TextFileSize
    CodePad = padding(CodeSize, TextFileSize),

    %% Data (or 1 zero byte if empty)
    ActualData = case DataSize of
        0 -> <<0>>;
        _ -> Data
    end,
    DataPad = padding(byte_size(ActualData), DataFileSize),

    %% Pad reloc
    RelocPad = padding(byte_size(RelocData), RelocFileSize),

    <<Headers/binary, HeaderPad/binary,
      Code/binary, CodePad/binary,
      ActualData/binary, DataPad/binary,
      RelocData/binary, RelocPad/binary>>.

%% ============================================================================
%% DOS Header — Minimal MZ stub that points to PE signature
%% ============================================================================

build_dos_header() ->
    %% Only two fields matter: e_magic = "MZ" and e_lfanew = offset to PE sig
    %% We place the PE signature right at offset 64 (end of DOS header).
    PEOffset = ?DOS_HEADER_SIZE,
    %% DOS header is 64 bytes. We fill in e_magic and e_lfanew, zero the rest.
    %% e_magic: offset 0, 2 bytes
    %% e_lfanew: offset 60, 4 bytes
    <<
      ?DOS_MAGIC/binary,       %% e_magic (2 bytes)
      0:(58*8),                %% zeros for rest of DOS header (58 bytes)
      PEOffset:32/little       %% e_lfanew (4 bytes) — offset to PE signature
    >>.

%% ============================================================================
%% COFF File Header (20 bytes)
%% ============================================================================

build_coff_header(Machine, NumSections) ->
    SizeOfOptionalHeader = ?OPTIONAL_HEADER_SIZE,
    Characteristics = ?IMAGE_FILE_EXECUTABLE_IMAGE bor
                      ?IMAGE_FILE_LARGE_ADDRESS_AWARE,
    <<
      Machine:16/little,
      NumSections:16/little,
      0:32/little,                   %% TimeDateStamp
      0:32/little,                   %% PointerToSymbolTable
      0:32/little,                   %% NumberOfSymbols
      SizeOfOptionalHeader:16/little,
      Characteristics:16/little
    >>.

%% ============================================================================
%% Optional Header PE32+ (240 bytes with 16 data directories)
%% ============================================================================

build_optional_header(EntryRVA, BaseOfCode, SizeOfHeaders,
                      SizeOfImage, Subsystem, DataDirectories) ->
    %% Standard fields (24 bytes)
    Standard = <<
      ?PE32PLUS_MAGIC:16/little,
      14:8,                          %% MajorLinkerVersion
      0:8,                           %% MinorLinkerVersion
      0:32/little,                   %% SizeOfCode (unused for our purposes)
      0:32/little,                   %% SizeOfInitializedData
      0:32/little,                   %% SizeOfUninitializedData
      EntryRVA:32/little,            %% AddressOfEntryPoint
      BaseOfCode:32/little           %% BaseOfCode
    >>,

    %% Windows-specific fields (88 bytes)
    DllCharacteristics = ?IMAGE_DLLCHARACTERISTICS_NX_COMPAT bor
                         ?IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE,
    WindowsFields = <<
      ?IMAGE_BASE_EXE:64/little,     %% ImageBase
      ?SECTION_ALIGNMENT:32/little,   %% SectionAlignment
      ?FILE_ALIGNMENT:32/little,      %% FileAlignment
      6:16/little,                    %% MajorOperatingSystemVersion
      0:16/little,                    %% MinorOperatingSystemVersion
      0:16/little,                    %% MajorImageVersion
      0:16/little,                    %% MinorImageVersion
      6:16/little,                    %% MajorSubsystemVersion
      0:16/little,                    %% MinorSubsystemVersion
      0:32/little,                    %% Win32VersionValue
      SizeOfImage:32/little,          %% SizeOfImage
      SizeOfHeaders:32/little,        %% SizeOfHeaders
      0:32/little,                    %% CheckSum
      Subsystem:16/little,            %% Subsystem
      DllCharacteristics:16/little,   %% DllCharacteristics
      16#100000:64/little,            %% SizeOfStackReserve (1MB)
      16#1000:64/little,              %% SizeOfStackCommit (4KB)
      16#100000:64/little,            %% SizeOfHeapReserve (1MB)
      16#1000:64/little,              %% SizeOfHeapCommit (4KB)
      0:32/little,                    %% LoaderFlags
      16:32/little                    %% NumberOfRvaAndSizes (16 directories)
    >>,

    %% Data directories are passed in (128 bytes = 16 * 8)
    <<Standard/binary, WindowsFields/binary, DataDirectories/binary>>.

%% ============================================================================
%% Data Directories (16 entries x 8 bytes = 128 bytes)
%% ============================================================================

build_data_directories(RelocRVA, RelocSize) ->
    %% 16 data directories, each {RVA:32, Size:32}
    %% Index 0: Export Table (empty)
    %% Index 1: Import Table (empty — UEFI apps don't use standard imports)
    %% Index 2: Resource Table (empty)
    %% Index 3: Exception Table (empty)
    %% Index 4: Certificate Table (empty)
    %% Index 5: Base Relocation Table
    %% Index 6-15: empty
    Dir0_4 = <<0:(5 * 64)>>,  %% 5 empty dirs (40 bytes)
    Dir5 = <<RelocRVA:32/little, RelocSize:32/little>>,
    Dir6_15 = <<0:(10 * 64)>>,  %% 10 empty dirs (80 bytes)
    <<Dir0_4/binary, Dir5/binary, Dir6_15/binary>>.

%% ============================================================================
%% Section Table (3 sections x 40 bytes = 120 bytes)
%% ============================================================================

build_section_table(
        TextRVA, TextVS, TextFO, TextFS,
        DataRVA, DataVS, DataFO, DataFS,
        RelocRVA, RelocVS, RelocFO, RelocFS) ->
    TextHdr = build_section_header(
        <<".text", 0, 0, 0>>,
        TextVS, TextRVA, TextFS, TextFO,
        ?IMAGE_SCN_CNT_CODE bor
        ?IMAGE_SCN_MEM_EXECUTE bor
        ?IMAGE_SCN_MEM_READ),
    DataHdr = build_section_header(
        <<".data", 0, 0, 0>>,
        DataVS, DataRVA, DataFS, DataFO,
        ?IMAGE_SCN_CNT_INITIALIZED_DATA bor
        ?IMAGE_SCN_MEM_READ bor
        ?IMAGE_SCN_MEM_WRITE),
    RelocHdr = build_section_header(
        <<".reloc", 0, 0>>,
        RelocVS, RelocRVA, RelocFS, RelocFO,
        ?IMAGE_SCN_CNT_INITIALIZED_DATA bor
        ?IMAGE_SCN_MEM_DISCARDABLE bor
        ?IMAGE_SCN_MEM_READ),
    <<TextHdr/binary, DataHdr/binary, RelocHdr/binary>>.

%% Build a single section header (40 bytes).
build_section_header(Name8, VirtualSize, VirtualAddress,
                     SizeOfRawData, PointerToRawData,
                     Characteristics) ->
    <<
      Name8/binary,                        %% Name (8 bytes, null-padded)
      VirtualSize:32/little,               %% VirtualSize (or Misc.PhysicalAddress)
      VirtualAddress:32/little,            %% VirtualAddress
      SizeOfRawData:32/little,             %% SizeOfRawData
      PointerToRawData:32/little,          %% PointerToRawData
      0:32/little,                         %% PointerToRelocations
      0:32/little,                         %% PointerToLinenumbers
      0:16/little,                         %% NumberOfRelocations
      0:16/little,                         %% NumberOfLinenumbers
      Characteristics:32/little            %% Characteristics
    >>.

%% ============================================================================
%% Base Relocation Section (minimal — empty block)
%% ============================================================================

%% Build a minimal .reloc section.
%% Contains one base relocation block with zero entries (just the header).
%% The block header is 8 bytes: PageRVA (4) + BlockSize (4).
%% BlockSize must be at least 8 (the header itself).
build_reloc_section(TextRVA) ->
    %% Minimal: one block with just the header, no entries.
    %% Some PE loaders require at least one block to exist.
    %% PageRVA must point to a valid page (not 0 / PE header).
    PageRVA = TextRVA,
    BlockSize = 8,  %% just the header, no entries
    <<PageRVA:32/little, BlockSize:32/little>>.

%% ============================================================================
%% Architecture Mapping
%% ============================================================================

arch_to_machine(x86_64) -> ?IMAGE_FILE_MACHINE_AMD64;
arch_to_machine(arm64) -> ?IMAGE_FILE_MACHINE_ARM64.

subsystem_value(efi) -> ?IMAGE_SUBSYSTEM_EFI_APPLICATION;
subsystem_value(windows) -> ?IMAGE_SUBSYSTEM_WINDOWS_CUI.

%% ============================================================================
%% Alignment Utilities
%% ============================================================================

file_align(Value) ->
    align_up(Value, ?FILE_ALIGNMENT).

section_align(Value) ->
    align_up(Value, ?SECTION_ALIGNMENT).

align_up(Value, Alignment) ->
    case Value rem Alignment of
        0 -> Value;
        R -> Value + (Alignment - R)
    end.

padding(CurrentSize, TargetSize) when TargetSize >= CurrentSize ->
    PadSize = TargetSize - CurrentSize,
    <<0:(PadSize * 8)>>;
padding(_, _) ->
    <<>>.

max(A, B) when A >= B -> A;
max(_, B) -> B.
