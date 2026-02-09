# ELF Relocatable Object Loader

**Location:** `os/compat/linux_ko_loader/vbeam_elf_loader.erl`

## Purpose

Parses and loads ELF64 relocatable objects (ET_REL type), including:
- Linux kernel modules (`.ko` files)
- Object files (`.o` files)
- Statically-linked libraries

This is the **inverse** of our ELF emitter (`vbeam_native_elf.erl`):
- **Emitter**: Generates ELF executables (ET_EXEC) from code/data sections
- **Loader**: Parses ELF relocatable objects (ET_REL) into code/data sections

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     ELF Loader Pipeline                      │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ 1. PARSE                                                     │
│    parse(Binary) -> {ok, ElfInfo}                           │
│                                                              │
│    • Read ELF64 header (magic, class, type, machine)        │
│    • Parse section headers (.text, .data, .symtab, etc.)    │
│    • Parse symbol table (name, type, bind, value)           │
│    • Parse relocation entries (.rela.text, .rela.data)      │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. RESOLVE SYMBOLS                                           │
│    resolve_symbols(ElfInfo, SymbolTable) -> {ok, Resolved}  │
│                                                              │
│    • Match undefined symbols (SHN_UNDEF) to symbol table    │
│    • Update resolved_addr field for external symbols        │
│    • Return list of unresolved symbols (if any)             │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. APPLY RELOCATIONS                                         │
│    apply_relocations(ElfInfo, BaseAddr) -> {ok, Code}       │
│                                                              │
│    • Assign addresses to allocated sections                 │
│    • Calculate relocation values (S + A - P)                │
│    • Patch section data with relocation values              │
│    • Concatenate all allocated sections                     │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ OUTPUT: Loaded Module                                        │
│    • code: Binary of executable code                        │
│    • data: Binary of initialized data                       │
│    • init_addr: Address of init_module/__init function      │
│    • exported_symbols: Map of symbol name -> address        │
└─────────────────────────────────────────────────────────────┘
```

## ELF Format Overview

### ELF64 Header (64 bytes)

```erlang
<<Magic:4/binary,           % 0x7F 'ELF'
  Class:8,                  % 2 = ELFCLASS64
  Endian:8,                 % 1 = little endian
  Version:8,                % 1 = EV_CURRENT
  ABI:8,                    % OS/ABI (0 = ELFOSABI_NONE)
  Pad:64,                   % Padding
  Type:16/little,           % 1 = ET_REL (relocatable)
  Machine:16/little,        % 0x3E = x86_64
  EVersion:32/little,       % 1 = EV_CURRENT
  Entry:64/little,          % Entry point (0 for .o files)
  PhOff:64/little,          % Program header offset (0 for .o)
  ShOff:64/little,          % Section header offset
  Flags:32/little,          % Architecture flags
  EhSize:16/little,         % ELF header size (64)
  PhEntSize:16/little,      % Program header size (0 for .o)
  PhNum:16/little,          % Program header count (0 for .o)
  ShEntSize:16/little,      % Section header size (64)
  ShNum:16/little,          % Section header count
  ShStrNdx:16/little>>      % .shstrtab section index
```

### Section Header (64 bytes)

```erlang
<<Name:32/little,           % Offset into .shstrtab
  Type:32/little,           % SHT_* constant
  Flags:64/little,          % SHF_* flags (ALLOC, WRITE, EXECINSTR)
  Addr:64/little,           % Virtual address (0 for .o)
  Offset:64/little,         % File offset
  Size:64/little,           % Section size in bytes
  Link:32/little,           % Link to related section
  Info:32/little,           % Extra info (depends on type)
  AddrAlign:64/little,      % Alignment requirement
  EntSize:64/little>>       % Entry size (for tables)
```

**Section Types:**
- `SHT_NULL (0)`: Null section
- `SHT_PROGBITS (1)`: Program data (.text, .data, .rodata)
- `SHT_SYMTAB (2)`: Symbol table
- `SHT_STRTAB (3)`: String table (.strtab, .shstrtab)
- `SHT_RELA (4)`: Relocation entries with addend (.rela.text)
- `SHT_NOBITS (8)`: Uninitialized data (.bss)

**Section Flags:**
- `SHF_WRITE (1)`: Writable
- `SHF_ALLOC (2)`: Occupies memory during execution
- `SHF_EXECINSTR (4)`: Executable code

### Symbol Table Entry (24 bytes)

```erlang
<<Name:32/little,           % Offset into .strtab
  Info:8,                   % Type (low 4 bits) + Bind (high 4 bits)
  Other:8,                  % Symbol visibility
  Shndx:16/little,          % Section index
  Value:64/little,          % Symbol value (offset in section)
  Size:64/little>>          % Symbol size
```

**Symbol Binding (Info >> 4):**
- `STB_LOCAL (0)`: Local symbol (file scope)
- `STB_GLOBAL (1)`: Global symbol (visible to all files)
- `STB_WEAK (2)`: Weak symbol (can be overridden)

**Symbol Type (Info & 0xF):**
- `STT_NOTYPE (0)`: No type
- `STT_OBJECT (1)`: Data object (variable)
- `STT_FUNC (2)`: Function
- `STT_SECTION (3)`: Section reference
- `STT_FILE (4)`: Source file name

**Special Section Indices:**
- `SHN_UNDEF (0)`: Undefined symbol (needs resolution)
- `SHN_ABS (0xFFF1)`: Absolute value (not section-relative)
- `SHN_COMMON (0xFFF2)`: Common symbol (like .bss)

### Relocation Entry with Addend (24 bytes)

```erlang
<<Offset:64/little,         % Offset in section to patch
  Info:64/little,           % Symbol index (high 32) + Type (low 32)
  Addend:64/signed-little>> % Addend value
```

**x86_64 Relocation Types:**
- `R_X86_64_NONE (0)`: No relocation
- `R_X86_64_64 (1)`: Direct 64-bit (S + A)
- `R_X86_64_PC32 (2)`: PC-relative 32-bit signed (S + A - P)
- `R_X86_64_PLT32 (4)`: PC-relative 32-bit PLT (L + A - P)
- `R_X86_64_32 (10)`: Direct 32-bit zero-extend (S + A)
- `R_X86_64_32S (11)`: Direct 32-bit sign-extend (S + A)

**Relocation Formula:**
- `S` = Symbol address
- `A` = Addend
- `P` = Place (address being patched)
- `L` = PLT entry address (for dynamic linking)

## Symbol Resolution

### Local Symbols (shndx > 0)

Local symbols are defined within the object file and reference a specific section:

```erlang
calculate_symbol_address(#{shndx := Shndx, value := Value}, AllSections, SectionAddrs) ->
    TargetSection = lists:nth(Shndx + 1, AllSections),
    SectionAddr = maps:get(TargetSection, SectionAddrs, 0),
    SectionAddr + Value.
```

**Example:**
```
Symbol: test_func
  shndx = 2 (.text section)
  value = 0x10 (offset within .text)

If .text is loaded at 0x1000000:
  Symbol address = 0x1000000 + 0x10 = 0x1000010
```

### External Symbols (shndx = SHN_UNDEF)

External symbols must be resolved against a symbol table:

```erlang
resolve_symbol(#{shndx := ?SHN_UNDEF, name := Name} = Sym, SymbolTable) ->
    case maps:find(Name, SymbolTable) of
        {ok, Addr} ->
            {ok, Sym#{resolved_addr := Addr}};
        error ->
            {error, Name}
    end.
```

**Example:**
```
Symbol: external_func
  shndx = SHN_UNDEF (0)
  value = 0

Symbol table provides: external_func -> 0x2000000
  Resolved address = 0x2000000
```

### Absolute Symbols (shndx = SHN_ABS)

Absolute symbols have a fixed value:

```erlang
calculate_symbol_address(#{shndx := ?SHN_ABS, value := Value}, _, _) ->
    Value.
```

## Relocation Process

### Step 1: Assign Section Addresses

Each allocated section (SHF_ALLOC flag set) is assigned an address:

```erlang
BaseAddr = 0x1000000

.text section (32 bytes):   0x1000000 .. 0x100001F
.data section (4 bytes):    0x1000020 .. 0x1000023
.eh_frame section (88 bytes): 0x1000030 .. 0x1000087
```

Sections are aligned to 16-byte boundaries for cache efficiency.

### Step 2: Calculate Relocation Values

For each relocation entry:

1. **Get symbol address** (S):
   - External symbol: Use `resolved_addr` from symbol table
   - Local symbol: Section address + symbol value
   - Absolute symbol: Symbol value directly

2. **Calculate place** (P):
   - Section address + relocation offset

3. **Apply relocation formula**:
   - `R_X86_64_64`: S + A
   - `R_X86_64_PC32`: S + A - P (mod 2^32)
   - `R_X86_64_PLT32`: S + A - P (mod 2^32)

**Example:**
```
Relocation in .text at offset 0x1A:
  type = R_X86_64_PLT32
  symbol = external_func (resolved to 0x2000000)
  addend = -4

P = 0x1000000 (.text base) + 0x1A = 0x100001A
S = 0x2000000 (external_func)
A = -4

Value = (S + A - P) mod 2^32
      = (0x2000000 - 4 - 0x100001A) mod 2^32
      = 0xFFFE2 mod 2^32
      = 0x00FFFE2

Patch 4 bytes at offset 0x1A with 0x00FFFE2 (little-endian)
```

### Step 3: Patch Section Data

Replace the 4 or 8 bytes at the relocation offset with the calculated value:

```erlang
patch_data(Data, Offset, Value, Type) ->
    WidthBytes = reloc_width(Type) div 8,
    <<Before:Offset/binary, _:WidthBytes/binary, After/binary>> = Data,
    Patch = case reloc_width(Type) of
        32 -> <<Value:32/little>>;
        64 -> <<Value:64/little>>
    end,
    <<Before/binary, Patch/binary, After/binary>>.
```

## API Reference

### parse/1

```erlang
parse(Binary :: binary()) -> {ok, elf_info()} | {error, term()}.
```

Parse an ELF binary into structured info.

**Returns:**
```erlang
#{
    header => #{
        class => 64,
        endian => little,
        type => rel,
        machine => x86_64,
        entry => 0,
        shoff => 1208,
        shnum => 11,
        shstrndx => 1
    },
    sections => [#{
        name => <<".text">>,
        type => progbits,
        flags => 6,  % SHF_ALLOC | SHF_EXECINSTR
        size => 32,
        data => <<...>>
    }, ...],
    symbols => [#{
        name => <<"test_func">>,
        bind => global,
        type => func,
        shndx => 2,
        value => 0,
        size => 11
    }, ...],
    relocations => #{
        <<".text">> => [#{
            offset => 26,
            type => r_x86_64_plt32,
            symbol => 5,
            addend => -4
        }]
    }
}
```

### resolve_symbols/2

```erlang
resolve_symbols(ElfInfo :: elf_info(),
                SymbolTable :: #{binary() => non_neg_integer()})
    -> {ok, elf_info()} | {error, [binary()]}.
```

Resolve undefined symbols against a symbol table.

**Example:**
```erlang
SymbolTable = #{
    <<"printk">> => 16#FFFFFFFF80100000,
    <<"kmalloc">> => 16#FFFFFFFF80200000
},
{ok, ResolvedElf} = resolve_symbols(ElfInfo, SymbolTable).
```

**Errors:**
```erlang
{error, [<<"undefined_func">>, <<"another_undefined">>]}
```

### apply_relocations/2

```erlang
apply_relocations(ElfInfo :: elf_info(), BaseAddr :: non_neg_integer())
    -> {ok, binary()} | {error, term()}.
```

Apply relocations and return concatenated code binary.

**Example:**
```erlang
BaseAddr = 16#1000000,
{ok, Code} = apply_relocations(ResolvedElf, BaseAddr).
% Code = <<85, 72, 137, 229, ...>>  % Relocated machine code
```

### load/2

```erlang
load(FilePath :: file:filename(),
     SymbolTable :: #{binary() => non_neg_integer()})
    -> {ok, loaded_module()} | {error, term()}.
```

High-level API: parse + resolve + relocate.

**Example:**
```erlang
SymbolTable = #{<<"external_func">> => 16#2000000},
{ok, Module} = vbeam_elf_loader:load("/tmp/test.o", SymbolTable),

#{
    code := Code,
    data := Data,
    init_addr := InitAddr,
    exported_symbols := Exports
} = Module.
```

## Testing

Run the test script to verify the loader:

```bash
cd os/compat/linux_ko_loader
./test_elf_loader.escript
```

**Test flow:**
1. Creates a simple C source file with functions and external references
2. Compiles to `.o` using gcc/clang (cross-compiles on macOS)
3. Parses the ELF binary
4. Prints sections, symbols, and relocations
5. Resolves external symbols
6. Applies relocations
7. Verifies the loaded code

**Expected output:**
```
=== ELF Loader Test ===

Creating test C source...
Compiling to object file...
  OK

Compiling loader module...
  Module loaded

=== Parsing ELF object ===
File size: 1328 bytes

--- ELF Header ---
  Type: rel
  Machine: x86_64
  Class: 64-bit
  Endian: little
  Sections: 11

--- Sections ---
  .text                progbits   size=    32 flags=XA
  .data                progbits   size=     4 flags=AW
  .symtab              symtab     size=   168 flags=
  ...

--- Symbols (7 total) ---
  test_func                      global   func     shndx=2      value=0x0 size=11
  caller                         global   func     shndx=2      value=0x10 size=16
  external_func                  global   notype   shndx=UNDEF  value=0x0 size=0
  ...

--- Relocations ---
  Section: .text (1 relocations)
    offset=0x1A type=r_x86_64_plt32   sym=5 addend=-4

=== Testing Symbol Resolution ===
All symbols resolved successfully

=== Testing Relocation ===
Base address: 0x1000000
Relocation successful
Loaded code size: 124 bytes

Test PASSED
```

## Use Cases

### Loading Linux Kernel Modules (.ko)

```erlang
%% Provide kernel symbol table
KernelSymbols = #{
    <<"printk">> => 16#FFFFFFFF80100000,
    <<"__kmalloc">> => 16#FFFFFFFF80200000,
    <<"module_layout">> => 16#FFFFFFFF80300000
},

{ok, Module} = vbeam_elf_loader:load("my_driver.ko", KernelSymbols),

#{code := Code, init_addr := InitAddr} = Module,

%% Execute init_module function at InitAddr
...
```

### Loading Static Libraries

```erlang
%% No external symbols needed for static library
{ok, Lib} = vbeam_elf_loader:load("libfoo.a.o", #{}),

#{exported_symbols := Exports} = Lib,

FooFunc = maps:get(<<"foo">>, Exports),
BarFunc = maps:get(<<"bar">>, Exports),
```

### Dynamic Linking (Future)

The loader can be extended to support:
- PLT (Procedure Linkage Table) generation
- GOT (Global Offset Table) for position-independent code
- Dynamic symbol resolution at runtime

## Limitations

### Current Implementation

- **x86_64 only**: ARM64, RISC-V not yet supported
- **Static relocation**: No PLT/GOT generation
- **No shared libraries**: ET_DYN not supported
- **Basic relocation types**: Only R_X86_64_64, PC32, PLT32, 32, 32S

### Future Enhancements

1. **Multi-architecture support**:
   - Add ARM64 relocation types (R_AARCH64_*)
   - Add RISC-V relocation types (R_RISCV_*)

2. **Dynamic linking**:
   - Generate PLT entries for lazy binding
   - Build GOT for position-independent code
   - Support ET_DYN shared objects

3. **Security**:
   - Verify signatures on kernel modules
   - Validate section permissions (W^X)
   - Check relocation bounds

4. **Performance**:
   - Parallel relocation processing
   - Memory-mapped file parsing
   - Lazy relocation on first use

## References

- **ELF64 Specification**: [System V ABI](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)
- **Linux Kernel Module Format**: [kernel.org - modules](https://www.kernel.org/doc/html/latest/kbuild/modules.html)
- **x86_64 Relocations**: [psABI Supplement](https://gitlab.com/x86-psABIs/x86-64-ABI)

## Related Files

- **ELF Emitter**: `vbeam_rt/src/vbeam_native_elf.erl` (generates ET_EXEC binaries)
- **Native Backend**: `vbeam_rt/src/vbeam_native.erl` (x86_64/ARM64 codegen)
- **BEAM Loader**: `erts/emulator/beam/beam_load.c` (Erlang's module loader)
