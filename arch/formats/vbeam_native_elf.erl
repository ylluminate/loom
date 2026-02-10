%%% @doc ELF64 executable emitter.
%%% Produces a statically-linked ET_EXEC ELF64 binary from raw code and data
%%% sections. Supports x86_64 and ARM64 architectures.
%%% @end
-module(vbeam_native_elf).

-export([emit/4]).

%% ============================================================================
%% ELF Constants
%% ============================================================================

-define(ELFMAG, <<16#7f, "ELF">>).
-define(ELFCLASS64, 2).
-define(ELFDATA2LSB, 1).       % Little endian
-define(EV_CURRENT, 1).
-define(ET_EXEC, 2).
-define(EM_X86_64, 16#3E).
-define(EM_AARCH64, 16#B7).
-define(PT_LOAD, 1).
-define(PF_X, 1).
-define(PF_W, 2).
-define(PF_R, 4).
-define(SHT_NULL, 0).
-define(SHT_PROGBITS, 1).
-define(SHT_STRTAB, 3).
-define(SHT_NOBITS, 8).
-define(SHF_WRITE, 1).
-define(SHF_ALLOC, 2).
-define(SHF_EXECINSTR, 4).

-define(PAGE_SIZE, 16#1000).   % 4096
-define(ELF_HEADER_SIZE, 64).
-define(PHDR_SIZE, 56).
-define(SHDR_SIZE, 64).

%% Standard load addresses
-define(TEXT_BASE, 16#400000).
-define(DATA_BASE, 16#600000).

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Emit a complete ELF64 binary.
%% Code: binary of machine code bytes for .text section.
%% Data: binary of initialized data bytes for .data section.
%% EntryOffset: byte offset within Code where execution begins.
%% Arch: x86_64 | arm64.
-spec emit(binary(), binary(), non_neg_integer(), x86_64 | arm64) -> binary().
emit(Code, Data, EntryOffset, Arch) ->
    Machine = arch_to_machine(Arch),

    %% Section name string table: \0 .text \0 .data \0 .bss \0 .shstrtab \0
    ShStrTab = build_shstrtab(),

    %% Layout calculation
    %% ELF header + 2 program headers
    NumPhdrs = 2,
    HeadersEnd = ?ELF_HEADER_SIZE + (NumPhdrs * ?PHDR_SIZE),

    %% Page-align the text segment within the file.
    %% The first LOAD segment must start at file offset 0 (covering the ELF
    %% header and program headers) so that Linux kernel's ELF loader and
    %% compatibility layers (OrbStack/Rosetta) can map the file correctly.
    %% Virtual address is TEXT_BASE; the code starts at TEXT_BASE + TextFileOff.
    TextFileOff = page_align(HeadersEnd),
    TextSize = byte_size(Code),
    TextVAddr = ?TEXT_BASE + TextFileOff,

    %% The first LOAD segment covers offset 0..TextFileOff+TextSize
    %% at virtual address TEXT_BASE, so the ELF header is mapped too.
    TextSegFileOff = 0,
    TextSegVAddr = ?TEXT_BASE,
    TextSegFileSz = TextFileOff + TextSize,
    TextSegMemSz = TextSegFileSz,

    %% Data segment follows text, page-aligned.
    %% Ensure data is at least 1 byte so that the LOAD segment has non-zero
    %% filesz/memsz (OrbStack's Rosetta translation fails with 0-byte segments).
    DataFileOff = page_align(TextFileOff + TextSize),
    DataPadded = case byte_size(Data) of
        0 -> <<0>>;
        _ -> Data
    end,
    DataSize = byte_size(DataPadded),
    DataVAddr = ?DATA_BASE,

    %% BSS follows data in virtual memory (no file backing)
    BssVAddr = DataVAddr + DataSize,
    BssSize = 0,  % No BSS content by default; caller can extend via linker

    %% .shstrtab follows data in the file
    ShStrTabOff = DataFileOff + DataSize,
    ShStrTabSize = byte_size(ShStrTab),

    %% Section headers follow .shstrtab, aligned to 8 bytes
    NumShdrs2 = 5, % null + .text + .data + .bss + .shstrtab
    ShOff = align(ShStrTabOff + ShStrTabSize, 8),

    %% Entry point virtual address
    true = EntryOffset < byte_size(Code),  %% Validate entry offset
    EntryVAddr = TextVAddr + EntryOffset,

    %% Build the components
    ElfHeader = build_elf_header(Machine, EntryVAddr, NumPhdrs, ShOff,
                                 NumShdrs2, shstrtab_index()),
    ProgramHeaders = build_program_headers(TextSegFileOff, TextSegVAddr,
                                           TextSegFileSz, TextSegMemSz,
                                           DataFileOff, DataVAddr, DataSize),

    %% Section headers
    SectionHeaders = build_section_headers(
        TextFileOff, TextVAddr, TextSize,
        DataFileOff, DataVAddr, DataSize,
        BssVAddr, BssSize,
        ShStrTabOff, ShStrTabSize),

    %% Assemble the binary
    HeaderBin = <<ElfHeader/binary, ProgramHeaders/binary>>,
    TextPad = padding(byte_size(HeaderBin), TextFileOff),
    DataPad = padding(TextFileOff + TextSize, DataFileOff),
    ShPad = padding(ShStrTabOff + ShStrTabSize, ShOff),

    <<HeaderBin/binary,
      TextPad/binary,
      Code/binary,
      DataPad/binary,
      DataPadded/binary,
      ShStrTab/binary,
      ShPad/binary,
      SectionHeaders/binary>>.

%% ============================================================================
%% ELF Header
%% ============================================================================

build_elf_header(Machine, EntryVAddr, NumPhdrs, ShOff, NumShdrs, ShStrNdx) ->
    EIdent = <<?ELFMAG/binary,
               ?ELFCLASS64:8,
               ?ELFDATA2LSB:8,
               ?EV_CURRENT:8,
               0:8,             % OS/ABI (ELFOSABI_NONE)
               0:64>>,          % ABI version + padding (8 bytes)
    <<EIdent/binary,
      ?ET_EXEC:16/little,
      Machine:16/little,
      ?EV_CURRENT:32/little,
      EntryVAddr:64/little,          % e_entry
      ?ELF_HEADER_SIZE:64/little,    % e_phoff (program headers right after ELF header)
      ShOff:64/little,               % e_shoff
      0:32/little,                   % e_flags
      ?ELF_HEADER_SIZE:16/little,    % e_ehsize
      ?PHDR_SIZE:16/little,          % e_phentsize
      NumPhdrs:16/little,            % e_phnum
      ?SHDR_SIZE:16/little,          % e_shentsize
      NumShdrs:16/little,            % e_shnum
      ShStrNdx:16/little>>.          % e_shstrndx

%% ============================================================================
%% Program Headers
%% ============================================================================

build_program_headers(TextSegFileOff, TextSegVAddr, TextSegFileSz, TextSegMemSz,
                      DataOff, DataVAddr, DataSize) ->
    %% First LOAD segment starts at file offset 0, covering ELF header +
    %% program headers + padding + .text code. This is required by the Linux
    %% kernel ELF loader and compatibility layers (OrbStack/Rosetta).
    TextPhdr = build_phdr(?PT_LOAD,
                          ?PF_R bor ?PF_X,
                          TextSegFileOff, TextSegVAddr, TextSegVAddr,
                          TextSegFileSz, TextSegMemSz,
                          ?PAGE_SIZE),
    DataPhdr = build_phdr(?PT_LOAD,
                          ?PF_R bor ?PF_W,
                          DataOff, DataVAddr, DataVAddr,
                          DataSize, DataSize,
                          ?PAGE_SIZE),
    <<TextPhdr/binary, DataPhdr/binary>>.

build_phdr(Type, Flags, Offset, VAddr, PAddr, FileSz, MemSz, Align) ->
    <<Type:32/little,
      Flags:32/little,
      Offset:64/little,
      VAddr:64/little,
      PAddr:64/little,
      FileSz:64/little,
      MemSz:64/little,
      Align:64/little>>.

%% ============================================================================
%% Section Headers
%% ============================================================================

build_section_headers(TextOff, TextVAddr, TextSize,
                      DataOff, DataVAddr, DataSize,
                      BssVAddr, BssSize,
                      ShStrTabOff, ShStrTabSize) ->
    %% Null section header (index 0)
    NullShdr = build_shdr(0, ?SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0),

    %% .text section (index 1)
    TextNameOff = shstrtab_offset(text),
    TextShdr = build_shdr(TextNameOff, ?SHT_PROGBITS,
                          ?SHF_ALLOC bor ?SHF_EXECINSTR,
                          TextVAddr, TextOff, TextSize,
                          0, 0, 16, 0),

    %% .data section (index 2)
    DataNameOff = shstrtab_offset(data),
    DataShdr = build_shdr(DataNameOff, ?SHT_PROGBITS,
                          ?SHF_ALLOC bor ?SHF_WRITE,
                          DataVAddr, DataOff, DataSize,
                          0, 0, 8, 0),

    %% .bss section (index 3)
    BssNameOff = shstrtab_offset(bss),
    BssShdr = build_shdr(BssNameOff, ?SHT_NOBITS,
                         ?SHF_ALLOC bor ?SHF_WRITE,
                         BssVAddr, 0, BssSize,
                         0, 0, 8, 0),

    %% .shstrtab section (index 4)
    ShStrNameOff = shstrtab_offset(shstrtab),
    ShStrShdr = build_shdr(ShStrNameOff, ?SHT_STRTAB,
                           0,
                           0, ShStrTabOff, ShStrTabSize,
                           0, 0, 1, 0),

    <<NullShdr/binary,
      TextShdr/binary,
      DataShdr/binary,
      BssShdr/binary,
      ShStrShdr/binary>>.

build_shdr(Name, Type, Flags, Addr, Offset, Size, Link, Info, AddrAlign, EntSize) ->
    <<Name:32/little,
      Type:32/little,
      Flags:64/little,
      Addr:64/little,
      Offset:64/little,
      Size:64/little,
      Link:32/little,
      Info:32/little,
      AddrAlign:64/little,
      EntSize:64/little>>.

%% ============================================================================
%% Section Name String Table (.shstrtab)
%% ============================================================================

%% Layout: \0 .text \0 .data \0 .bss \0 .shstrtab \0
%% Offsets: 0   1       7      13    18

build_shstrtab() ->
    <<0,                              % offset 0: null string
      ".text", 0,                     % offset 1
      ".data", 0,                     % offset 7
      ".bss", 0,                      % offset 13
      ".shstrtab", 0>>.              % offset 18

shstrtab_offset(text)     -> 1;
shstrtab_offset(data)     -> 7;
shstrtab_offset(bss)      -> 13;
shstrtab_offset(shstrtab) -> 18.

%% .shstrtab is section index 4 (null=0, .text=1, .data=2, .bss=3, .shstrtab=4)
shstrtab_index() -> 4.

%% ============================================================================
%% Architecture Mapping
%% ============================================================================

arch_to_machine(x86_64) -> ?EM_X86_64;
arch_to_machine(arm64)  -> ?EM_AARCH64.

%% ============================================================================
%% Alignment Utilities
%% ============================================================================

%% @doc Align value up to the given boundary.
-spec align(non_neg_integer(), pos_integer()) -> non_neg_integer().
align(Value, Boundary) ->
    case Value rem Boundary of
        0 -> Value;
        R -> Value + (Boundary - R)
    end.

%% @doc Align value up to the page boundary.
-spec page_align(non_neg_integer()) -> non_neg_integer().
page_align(Value) ->
    align(Value, ?PAGE_SIZE).

%% @doc Generate padding bytes to go from CurrentOff to TargetOff.
-spec padding(non_neg_integer(), non_neg_integer()) -> binary().
padding(CurrentOff, TargetOff) when TargetOff >= CurrentOff ->
    PadSize = TargetOff - CurrentOff,
    <<0:(PadSize * 8)>>;
padding(CurrentOff, TargetOff) ->
    error({padding_underflow, CurrentOff, TargetOff}).
