# Sketch: Loom Project Migration

COVERS:
- os/** (nucleus, beam_vm, kernel, compat)
- vbeam_rt/src/** (native backends)
- tests/native/** (native test suite)
- loom/** (presentation materials)
- docs/ (nucleus/native-related)
- scripts/ (nucleus/native test scripts)

## Current State: Everything in vbeam

```mermaid
flowchart TD
    subgraph "vbeam__v_on_beam (CURRENT - mixed)"
        subgraph "COMPILER (stays)"
            V1[v1 BEAM compiler<br/>coregen.v, core_*.v]
            V2[v2 BEAM compiler<br/>vlang-v2/beam/beam.v]
            RT[runtime/ .erl modules]
            TR[tests/runtime/ 57 tests]
            DEMOS[demos/ + scripts/demo_*]
            BENCH[scripts/bench.sh]
        end

        subgraph "LOOM OS (moves)"
            NUC[os/nucleus/<br/>boot.erl, fonts, ESP]
            BVM[os/beam_vm/<br/>interpreters, parser]
            KRN[os/kernel/<br/>GDT, paging, heap, scheduler]
            CMP[os/compat/<br/>syscalls, ELF, LinuxKPI]
            NAT[vbeam_rt/src/<br/>x86_64, ARM64, IR, PE/ELF/Mach-O]
            TNT[tests/native/ 9 tests]
            TNV[test_native_*.v 5 files]
            LOOM[loom/ submission materials]
            NDOC[docs/ nucleus+native docs]
            NSCP[scripts/test_nucleus*.sh<br/>scripts/test_native*.sh]
        end
    end
```

## After Migration: Clean Separation

```mermaid
flowchart LR
    subgraph "vbeam__v_on_beam (compiler only)"
        V1B[v1 BEAM compiler]
        V2B[v2 BEAM compiler<br/>46/46 + 57/57 tests]
        RTB[runtime/ .erl modules]
        TRB[tests/runtime/ 57 tests]
        DMB[demos/ fault tolerance<br/>hot reload, spawn]
        SCB[scripts/ bench, demo, test_runtime]
    end

    subgraph "loom (OS nucleus)"
        direction TB
        subgraph "src/"
            NUC2[nucleus/<br/>boot.erl, fonts]
            BVM2[beam_vm/<br/>interpreters, parser]
            KRN2[kernel/<br/>GDT, paging, scheduler]
            CMP2[compat/<br/>syscalls, ELF, LinuxKPI]
            NAT2[native/<br/>x86_64, ARM64, IR, linker<br/>PE, ELF, Mach-O emitters]
        end
        subgraph "tests/"
            TST2[native/ 9 V test programs]
        end
        subgraph "docs/"
            DOC2[architecture, boot sequence<br/>interpreter, syscalls, testing]
        end
        subgraph "meta/"
            SUB2[submission materials<br/>README, CHECKLIST, video script]
        end
        MK2[Makefile<br/>nucleus, qemu, test targets]
    end

    vbeam__v_on_beam -.->|"V compiles to<br/>BEAM/native"| loom
```

## Migration Steps

```mermaid
flowchart TD
    S1[1. Create ~/tank/ventures/v_projects/loom/] --> S2
    S2[2. Init git repo + directory structure] --> S3
    S3[3. Copy files with structure mapping] --> S4
    S4[4. Create Makefile, README, AI.md] --> S5
    S5[5. Verify builds: make nucleus] --> S6
    S6[6. Verify tests: native test suite] --> S7
    S7[7. Remove files from vbeam] --> S8
    S8[8. Verify vbeam still works: 57/57] --> S9
    S9[9. Commit both repos]
```

## Directory Mapping

| Source (vbeam) | Target (loom) |
|---|---|
| `os/nucleus/` | `src/nucleus/` |
| `os/beam_vm/` | `src/beam_vm/` |
| `os/kernel/` | `src/kernel/` |
| `os/compat/` | `src/compat/` |
| `vbeam_rt/src/vbeam_native*.erl` | `src/native/` |
| `tests/native/` | `tests/native/` |
| `test_native_*.v` | `tests/native/` (consolidated) |
| `loom/` | `meta/` (submission materials) |
| `docs/nucleus-*.md` | `docs/` |
| `docs/beam-interpreter.md` | `docs/` |
| `docs/native-*.md` | `docs/` |
| `docs/elf-ko-loader.md` | `docs/` |
| `docs/linux-syscall-table.md` | `docs/` |
| `docs/linuxkpi-surface.md` | `docs/` |
| `scripts/test_nucleus_qemu.sh` | `scripts/` |
| `scripts/test_native_*.sh` | `scripts/` |
| `scripts/render_font_bitmap.py` | `scripts/` |
| `thoughts/sketches/*nucleus*` | `thoughts/sketches/` |
| `thoughts/sketches/*native*` | `thoughts/sketches/` |
| `thoughts/sketches/*loom*` | `thoughts/sketches/` |
| `preview_monaspice.png` | `assets/` |
| Makefile nucleus targets | `Makefile` (standalone) |

## What Must NOT Break

- vbeam 57/57 runtime tests must still pass after removal
- vbeam demos (fault tolerance, hot reload, spawn) must still work
- `make nucleus` must work from loom directory
- QEMU boot must work from loom directory
- Native test scripts must find their modules

## How I'll Verify

- [ ] `cd loom && make nucleus` builds nucleus.efi
- [ ] `cd loom && make qemu-test` boots (if QEMU available)
- [ ] `cd vbeam && bash scripts/test_runtime.sh` still 57/57
- [ ] `cd vbeam && bash demos/hackathon_showcase.sh` still runs
- [ ] All .erl files compile with erlc in new location
