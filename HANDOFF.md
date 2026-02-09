# HANDOFF

Last updated (UTC): 2026-02-09T17:00:00Z

## Objective (1 sentence)

Loom OS — a research operating system where BEAM runs on bare metal, with UEFI boot, supervised fault tolerance, and V-to-native compilation.

## Current status

### What Just Happened
- **OS-style directory restructure** (2026-02-09): Reorganized from flat `src/` dump into proper subsystem hierarchy following Linux/SerenityOS conventions.
- **Modern Makefile** with automatic source discovery, per-subsystem targets, NO_COLOR support, VERBOSE mode, `_build/` output.
- **Per-subsystem READMEs** for boot/, kernel/, vm/, arch/, compat/.
- **All docs updated** to reflect new directory structure.

### What Works
- `make compile` → 33/33 modules OK
- `make nucleus` → 5120 bytes
- `make help` → shows all targets with descriptions
- `make info` → shows module counts and build status
- `make test-kernel` / `make test-vm` → run subsystem tests

### Directory Structure
```
boot/                 UEFI boot (PE32+, fonts, framebuffer)
kernel/{boot,mm,sched,io,arch}    Core kernel subsystems
vm/{interp,parser,jit}            BEAM virtual machine
arch/{x86_64,arm64,ir,link,formats}  Native backends
compat/{syscall,elf,kpi}          Linux compatibility
tests/{kernel,vm,native,data}     All tests separated from source
tools/                            Scripts and utilities
```

### Key Files
| File | What |
|------|------|
| `boot/vbeam_nucleus_boot.erl` | UEFI PE32+ boot (~738 LOC) |
| `vm/interp/vbeam_beam_interp_v2.erl` | BEAM interpreter (~530 LOC) |
| `vm/parser/vbeam_beam_standalone.erl` | Standalone parser (no OTP) |
| `kernel/sched/vbeam_scheduler.erl` | Preemptive scheduler |
| `arch/x86_64/vbeam_native_x86_64.erl` | x86_64 backend (53 privileged insns) |
| `Makefile` | Modern build system (~260 LOC) |

## What's Next

1. **Hackathon (Feb 10-16)** — demo pipeline: V → BEAM → bare metal
2. **Bare-metal BEAM interpreter** — remove remaining OTP deps from standalone parser
3. **Self-hosting milestone** — kernel compiles V code via its own BEAM interpreter
4. **Driver framework** — supervised driver processes with hot reload

## Blockers

- None currently — build verified working after restructure.
