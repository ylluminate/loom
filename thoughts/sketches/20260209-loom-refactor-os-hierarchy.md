# Sketch: Loom OS Directory Refactor

COVERS:
- All files under src/ (46 .erl modules)
- tests/ reorganization
- scripts/ consolidation
- Build system overhaul

## Current State (flat dump from migration)

```mermaid
flowchart TD
    subgraph "src/ (FLAT - problems)"
        N[nucleus/<br/>3 .erl + fonts + ESP + .beam]
        BV[beam_vm/<br/>9 .erl + tests + testdata + .beam]
        K[kernel/<br/>18 .erl = 9 source + 9 tests + .beam]
        C[compat/<br/>4 .erl + escript + .beam]
        NA[native/<br/>12 .erl all flat + .beam]
    end

    style N fill:#fcc
    style K fill:#fcc
    style BV fill:#fcc
    style NA fill:#fcc
```

**Problems:**
1. Tests mixed with source (kernel/ has 9 test files)
2. .beam artifacts scattered in source dirs
3. Flat namespaces (native/ has 12 unrelated files)
4. No subsystem READMEs
5. Scripts scattered across subsystems
6. No clear "where does X go?" answer

## Target State (OS-style hierarchy)

```mermaid
flowchart TD
    subgraph "boot/ — UEFI Boot"
        B1[vbeam_nucleus_boot.erl]
        B2[fonts/<br/>vbeam_font_8x16.erl<br/>vbeam_font_monaspice.erl]
        B3[esp/<br/>EFI/BOOT/BOOTX64.EFI]
    end

    subgraph "kernel/ — Core Kernel"
        K1[boot/<br/>vbeam_boot_sequence.erl]
        K2[mm/<br/>vbeam_page_alloc.erl<br/>vbeam_paging.erl<br/>vbeam_heap.erl]
        K3[sched/<br/>vbeam_scheduler.erl]
        K4[io/<br/>vbeam_io_server.erl<br/>vbeam_irq_bridge.erl]
        K5[arch/<br/>vbeam_gdt_idt.erl]
    end

    subgraph "vm/ — BEAM Virtual Machine"
        V1[interp/<br/>vbeam_beam_interp.erl<br/>vbeam_beam_interp_v2.erl<br/>vbeam_beam_interp_bare.erl]
        V2[parser/<br/>vbeam_beam_parser.erl<br/>vbeam_beam_standalone.erl]
        V3[jit/<br/>vbeam_beam_to_native.erl]
    end

    subgraph "arch/ — Native Backends"
        A1[x86_64/<br/>vbeam_native_x86_64.erl<br/>vbeam_native_lower_x86_64.erl]
        A2[arm64/<br/>vbeam_native_arm64.erl<br/>vbeam_native_lower_arm64.erl]
        A3[ir/<br/>vbeam_native_ir.erl<br/>vbeam_native_regalloc.erl<br/>vbeam_native_alloc.erl]
        A4[link/<br/>vbeam_native_link.erl<br/>vbeam_native.erl]
        A5[formats/<br/>vbeam_native_pe.erl<br/>vbeam_native_elf.erl<br/>vbeam_native_macho.erl]
    end

    subgraph "compat/ — Linux Compatibility"
        C1[syscall/<br/>vbeam_linux_syscall.erl]
        C2[elf/<br/>vbeam_elf_loader.erl]
        C3[kpi/<br/>vbeam_linuxkpi.erl<br/>vbeam_kapi_symbols.erl]
    end

    subgraph "tests/ — All Tests"
        T1[kernel/<br/>*_test.erl files]
        T2[vm/<br/>test_*.erl + testdata/]
        T3[native/<br/>V test programs]
    end
```

## File Mapping

| From | To | Why |
|------|-----|-----|
| `src/nucleus/vbeam_nucleus_boot.erl` | `boot/vbeam_nucleus_boot.erl` | Boot is its own top-level subsystem |
| `src/nucleus/vbeam_font_*.erl` | `boot/fonts/` | Fonts are boot-only |
| `src/nucleus/esp/` | `boot/esp/` | ESP partition structure |
| `src/kernel/vbeam_boot_sequence.erl` | `kernel/boot/` | Boot orchestration subsystem |
| `src/kernel/vbeam_page_alloc.erl` | `kernel/mm/` | Memory management |
| `src/kernel/vbeam_paging.erl` | `kernel/mm/` | Memory management |
| `src/kernel/vbeam_heap.erl` | `kernel/mm/` | Memory management |
| `src/kernel/vbeam_scheduler.erl` | `kernel/sched/` | Scheduler subsystem |
| `src/kernel/vbeam_io_server.erl` | `kernel/io/` | I/O subsystem |
| `src/kernel/vbeam_irq_bridge.erl` | `kernel/io/` | IRQ is part of I/O |
| `src/kernel/vbeam_gdt_idt.erl` | `kernel/arch/` | Architecture-specific |
| `src/kernel/vbeam_beam_to_native.erl` | `vm/jit/` | JIT is VM concern |
| `src/beam_vm/vbeam_beam_interp*.erl` | `vm/interp/` | Interpreter subsystem |
| `src/beam_vm/vbeam_beam_parser.erl` | `vm/parser/` | Parser subsystem |
| `src/beam_vm/vbeam_beam_standalone.erl` | `vm/parser/` | Parser subsystem |
| `src/beam_vm/*.escript` | `tools/` | Utility scripts |
| `src/native/vbeam_native_x86_64.erl` | `arch/x86_64/` | Arch-specific |
| `src/native/vbeam_native_lower_x86_64.erl` | `arch/x86_64/` | Arch-specific |
| `src/native/vbeam_native_arm64.erl` | `arch/arm64/` | Arch-specific |
| `src/native/vbeam_native_lower_arm64.erl` | `arch/arm64/` | Arch-specific |
| `src/native/vbeam_native_ir.erl` | `arch/ir/` | IR shared across arches |
| `src/native/vbeam_native_regalloc.erl` | `arch/ir/` | Regalloc shared |
| `src/native/vbeam_native_alloc.erl` | `arch/ir/` | Alloc shared |
| `src/native/vbeam_native_link.erl` | `arch/link/` | Linker shared |
| `src/native/vbeam_native.erl` | `arch/link/` | Entry point |
| `src/native/vbeam_native_pe.erl` | `arch/formats/` | Object format |
| `src/native/vbeam_native_elf.erl` | `arch/formats/` | Object format |
| `src/native/vbeam_native_macho.erl` | `arch/formats/` | Object format |
| `src/compat/vbeam_linux_syscall.erl` | `compat/syscall/` | Syscall subsystem |
| `src/compat/vbeam_elf_loader.erl` | `compat/elf/` | ELF loader |
| `src/compat/vbeam_linuxkpi.erl` | `compat/kpi/` | KPI shims |
| `src/compat/vbeam_kapi_symbols.erl` | `compat/kpi/` | KPI symbols |
| `src/compat/test_elf_loader.escript` | `tools/` | Utility script |
| All `*_test.erl` from kernel | `tests/kernel/` | Tests separated |
| All `test_*.erl` from beam_vm | `tests/vm/` | Tests separated |
| `src/beam_vm/testdata/` | `tests/data/` | Test data |
| All scripts/* | `tools/` | Consolidated |

## What Must NOT Break

- `make nucleus` must build 5120-byte nucleus.efi
- All kernel test modules must compile and pass
- .erl files must be findable by Makefile for compilation
- ESP partition structure must remain intact

## Build System Best Practices

- Automatic dependency discovery (find all .erl files)
- Parallel compilation (-j support)
- Per-subsystem targets (make boot, make kernel, make vm, etc.)
- Color output with [OK]/[FAIL] markers
- make help with aligned descriptions
- make test runs all test suites
- make clean / make distclean separation
- Proper .PHONY declarations
- Variable overrides for cross-compilation
