# Sketch: Phase 6 — Native Backend Data Types (strings, arrays, structs, maps, floats, methods)

COVERS:
- vbeam_rt/src/vbeam_native_alloc.erl (NEW)
- vbeam_rt/src/vbeam_native_ir.erl
- vbeam_rt/src/vbeam_native_lower_arm64.erl
- vbeam_rt/src/vbeam_native_lower_x86_64.erl
- vbeam_rt/src/vbeam_native_arm64.erl (minor additions)
- vbeam_rt/src/vbeam_native_x86_64.erl (minor additions)
- vbeam_rt/src/vbeam_native.erl (alloc init injection)
- vbeam_rt/src/vbeam_native_regalloc.erl (heap reg reservation + needs_heap fix)
- vbeam_rt/test/strings_arm64.ir (NEW test)
- vbeam_rt/test/arrays_arm64.ir (NEW test)
- vbeam_rt/test/structs_arm64.ir (NEW test)
- vbeam_rt/test/hello_arm64.ir (existing, regression test)

## Current State

```mermaid
flowchart TD
    subgraph "V AST → IR (irgen.v)"
        AST[V AST] --> IRGen[irgen.v]
        IRGen --> IR[IR Instructions]
    end

    subgraph "IR Opcodes (vbeam_native_ir.erl)"
        IR --> Opcodes{Current opcodes}
        Opcodes --> IntOps[mov, mov_imm, add, sub, mul, sdiv, srem]
        Opcodes --> BitOps[and_, or_, xor_, shl, shr, sar, neg, not_]
        Opcodes --> MemOps[load, store, lea]
        Opcodes --> CtrlOps[cmp, jmp, jcc, call, ret]
        Opcodes --> IOOps[print_int, syscall]
    end

    subgraph "Pipeline (vbeam_native.erl)"
        IR --> RegAlloc[vbeam_native_regalloc]
        RegAlloc --> Lower{Target?}
        Lower -->|arm64| ARM64Lower[vbeam_native_lower_arm64]
        Lower -->|x86_64| X86Lower[vbeam_native_lower_x86_64]
        ARM64Lower --> ARM64Enc[vbeam_native_arm64 encoder]
        X86Lower --> X86Enc[vbeam_native_x86_64 encoder]
        ARM64Enc --> Link[vbeam_native_link]
        X86Enc --> Link
        Link --> Binary{Format?}
        Binary -->|elf64| ELF[vbeam_native_elf]
        Binary -->|macho| Macho[vbeam_native_macho]
        Binary -->|pe| PE[vbeam_native_pe]
    end

    subgraph "Register Allocation"
        RegAlloc --> Available["Available regs:<br/>ARM64: x0-x18, x19-x28<br/>x86_64: rax-r11, rbx, r12-r15<br/><br/>Reserved:<br/>ARM64: x29(fp), x30(lr), sp<br/>x86_64: rbp(fp), rsp(sp)"]
    end
```

## What I'm Changing

### New Module: vbeam_native_alloc.erl (bump allocator)

```mermaid
flowchart TD
    subgraph "Bump Allocator"
        Init[alloc_init] -->|mmap 1MB| Heap[Heap memory]
        Heap --> HeapPtr["Heap pointer<br/>ARM64: x28<br/>x86_64: r15"]
        Alloc[alloc N bytes] --> RetPtr[Return current ptr]
        RetPtr --> Bump["Advance ptr by N"]
    end

    subgraph "Memory Layout"
        HeapBase["heap_base (from mmap)"] --> Used["Used memory<br/>(strings, arrays, etc)"]
        Used --> Ptr["← heap_ptr"]
        Ptr --> Free["Free space"]
        Free --> End["heap_base + 1MB"]
    end
```

### New IR Opcodes (added to vbeam_native_ir.erl)

```mermaid
flowchart LR
    subgraph "String Opcodes"
        SL[string_lit Dst Bytes] --> SLen[string_len Dst Src]
        SLen --> SCmp[string_cmp Dst A B]
        SCmp --> SConcat[string_concat Dst A B]
        SConcat --> SPrint[print_str Src]
    end

    subgraph "Array Opcodes"
        ANew[array_new Dst ElemSize Cap] --> AGet[array_get Dst Arr Idx]
        AGet --> ASet[array_set Arr Idx Val]
        ASet --> ALen[array_len Dst Arr]
        ALen --> AApp[array_append Dst Arr Val]
    end

    subgraph "Struct Opcodes"
        StNew[struct_new Dst Size] --> FGet[field_get Dst Struct Off]
        FGet --> FSet[field_set Struct Off Val]
    end

    subgraph "Map Opcodes"
        MNew[map_new Dst] --> MGet[map_get Dst Map Key]
        MGet --> MPut[map_put Dst Map Key Val]
        MPut --> MDel[map_delete Dst Map Key]
    end

    subgraph "Float Opcodes"
        FAdd[fadd Dst A B] --> FSub[fsub Dst A B]
        FSub --> FMul[fmul Dst A B]
        FMul --> FDiv[fdiv Dst A B]
        FDiv --> I2F[int_to_float Dst Src]
        I2F --> F2I[float_to_int Dst Src]
        F2I --> FPrint[print_float Src]
    end

    subgraph "Method Call"
        MC[method_call Dst Type Method Args]
    end
```

### Data Representations

```mermaid
flowchart TD
    subgraph "String (fat pointer, 16 bytes)"
        StrPtr["ptr: 8 bytes → data bytes"]
        StrLen["len: 8 bytes"]
    end

    subgraph "Array (24 bytes header)"
        ArrPtr["ptr: 8 bytes → element data"]
        ArrLen2["len: 8 bytes"]
        ArrCap["cap: 8 bytes"]
    end

    subgraph "Struct (N bytes)"
        Field1["field0: at offset 0"]
        Field2["field1: at offset 8"]
        FieldN["fieldN: at offset N*8"]
    end

    subgraph "Map (sorted array, 24 bytes header)"
        MapPtr["entries_ptr: 8 bytes → key-value pairs"]
        MapLen3["len: 8 bytes"]
        MapCap2["cap: 8 bytes"]
    end
```

### Pipeline Change (vbeam_native.erl)

```mermaid
flowchart TD
    Compile[do_compile] --> AllocInit["NEW: emit alloc_init code<br/>before first function body"]
    AllocInit --> RegAlloc[Register allocate]
    RegAlloc --> Lower[Lower to machine code]
    Lower --> Link[Link + relocate]
    Link --> Emit[Emit binary]

    subgraph "RegAlloc Change"
        Reserve["Reserve x28/r15 as heap_reg<br/>Remove from available pool"]
    end
```

## What Must NOT Break
- Existing int arithmetic programs (hello, fib, ifelse, break, logic)
- Register allocation for existing programs (x28/r15 removal from pool)
- ARM64 Mach-O output format
- x86_64 ELF output format
- Relocation resolution for branches and data refs
- print_int functionality

## How I'll Verify It Works
- [ ] `erlc` compiles all modified .erl files without errors
- [ ] Existing test_native_hello.v still compiles and runs correctly
- [ ] New string_lit IR instruction produces correct data section entries
- [ ] print_str outputs string to stdout via write syscall
- [ ] array_new + array_get produces correct indexed access
- [ ] struct_new + field_get/field_set work with byte offsets
