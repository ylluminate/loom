# Sketch: Native Machine Code Emission Backend (x86_64 + ARM64)

COVERS:
- vbeam_rt/src/vbeam_native.erl
- vbeam_rt/src/vbeam_native_ir.erl
- vbeam_rt/src/vbeam_native_regalloc.erl
- vbeam_rt/src/vbeam_native_x86_64.erl
- vbeam_rt/src/vbeam_native_arm64.erl
- vbeam_rt/src/vbeam_native_lower_x86_64.erl
- vbeam_rt/src/vbeam_native_lower_arm64.erl
- vbeam_rt/src/vbeam_native_elf.erl
- vbeam_rt/src/vbeam_native_macho.erl
- vbeam_rt/src/vbeam_native_pe.erl
- vbeam_rt/src/vbeam_native_link.erl
- vbeam_rt/test/hello_x86_64.ir
- vbeam_rt/test/hello_arm64.ir
- vbeam_rt/test/test_native.erl

## Current State (existing BEAM pipeline)
```mermaid
flowchart TD
    VS[V Source .v] --> VC[V Compiler<br>v -b beam]
    VC --> CE[Core Erlang .core text]
    CE --> VBC[vbeam_compiler.erl<br>escript on BEAM]
    VBC --> |core_scan + core_parse| AST[Core Erlang AST]
    AST --> |compile:forms/2| BEAM[.beam bytecode]
    BEAM --> VM[BEAM VM<br>erl -pa .]
    VM --> OUT[Program Output]

    subgraph "Runtime (vbeam_rt/src/)"
        RT1[vbeam_io.erl]
        RT2[vbeam_string.erl]
        RT3[vbeam_array.erl]
        RT4[vbeam_math.erl]
        RT5[vbeam_conv.erl]
        RT6[vbeam_panic.erl]
    end
    VM -.-> RT1
```

## New State (adding native code path)
```mermaid
flowchart TD
    VS[V Source .v] --> VC[V Compiler<br>v -b beam]

    VC --> |--target beam| CE[Core Erlang .core]
    CE --> VBC[vbeam_compiler.erl]
    VBC --> BEAM[.beam bytecode]

    VC --> |--target native-x86_64<br>--target native-arm64| IR[IR Terms File<br>.ir Erlang terms]
    IR --> VN[vbeam_native.erl<br>escript entry point]

    VN --> IRMOD[vbeam_native_ir.erl<br>parse + validate]
    IRMOD --> RA[vbeam_native_regalloc.erl<br>linear scan]

    RA --> |x86_64| LX[vbeam_native_lower_x86_64.erl<br>IR → x86_64 instructions]
    RA --> |arm64| LA[vbeam_native_lower_arm64.erl<br>IR → ARM64 instructions]

    LX --> EX[vbeam_native_x86_64.erl<br>encode machine bytes]
    LA --> EA[vbeam_native_arm64.erl<br>encode machine bytes]

    EX --> LK[vbeam_native_link.erl<br>resolve relocations]
    EA --> LK

    LK --> |Linux| ELF[vbeam_native_elf.erl<br>ELF64 binary]
    LK --> |macOS| MACHO[vbeam_native_macho.erl<br>Mach-O binary]
    LK --> |UEFI| PE[vbeam_native_pe.erl<br>PE binary]

    ELF --> BIN[Native Executable]
    MACHO --> BIN
    PE --> BIN
```

## Data Flow Detail
```mermaid
flowchart LR
    subgraph IR["IR Layer (vbeam_native_ir)"]
        OP[Operands<br>vreg/preg/stack/imm]
        INST[Instructions<br>mov/add/call/ret/...]
        FN[Functions<br>name + body + params]
        MOD[Module<br>target + format + fns + data]
    end

    subgraph RA["Register Allocation"]
        LI[Live Intervals] --> LS[Linear Scan]
        LS --> PM[Physical Register Map]
        LS --> SP[Spill Slots]
    end

    subgraph ENC["Encoding (per-arch)"]
        REX[x86_64: REX+Opcode+ModR/M]
        ARM[ARM64: Fixed 32-bit words]
    end

    subgraph BIN["Binary Output"]
        CODE[.text section<br>machine code bytes]
        DATA[.data section<br>initialized data]
        BSS[.bss section<br>uninitialized]
        SYM[Symbol table]
        REL[Relocations]
    end

    MOD --> LI
    PM --> REX
    PM --> ARM
    REX --> CODE
    ARM --> CODE
    CODE --> REL
```

## What I'm Changing
Adding 11 new Erlang modules to vbeam_rt/src/ that implement a complete native code emission pipeline. These modules run entirely on BEAM and produce standalone executables for x86_64 and ARM64.

## What Must NOT Break
- Existing BEAM compilation pipeline (Core Erlang path) is untouched
- Existing runtime modules (vbeam_io, vbeam_string, etc.) are not modified
- 21/21 runtime tests continue to pass
- 235/249 V examples continue to compile

## How I'll Verify It Works
- [x] IR module: parse/validate/pretty-print a test IR file
- [x] x86_64 encoder: verify MOV RAX,42 produces correct bytes (48 C7 C0 2A000000)
- [x] ARM64 encoder: verify ADD X0,X1,X2 produces correct 32-bit word
- [ ] ELF64: produce valid ELF header (check with hexdump)
- [ ] Mach-O: produce valid Mach-O header (check with otool or hexdump)
- [ ] Hello world: emit binary that prints "Hello" via syscall
- [ ] Fibonacci: emit binary that computes fib(35)=9227465
- [ ] Existing tests: ./scripts/test_runtime.sh still passes 21/21

## Integration Test Plan
```mermaid
flowchart TD
    IR1[hello_x86_64.ir<br>x86_64 ELF64 test] --> PARSE[vbeam_native_ir:parse_file]
    IR2[hello_arm64.ir<br>ARM64 Mach-O test] --> PARSE
    PARSE --> VAL[validate_module]
    VAL --> ALLOC[regalloc:allocate]
    ALLOC --> LOW{target?}
    LOW -->|x86_64| LX[lower_x86_64]
    LOW -->|arm64| LA[lower_arm64]
    LX --> ENC_X[x86_64 encoder]
    LA --> ENC_A[arm64 encoder]
    ENC_X --> LINK[vbeam_native_link:resolve]
    ENC_A --> LINK
    LINK --> FMT{format?}
    FMT -->|elf64| ELF[ELF64 binary<br>hexdump verify]
    FMT -->|macho| MACHO[Mach-O binary<br>otool verify + execute]
```
