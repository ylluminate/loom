# HANDOFF

Last updated (UTC): 2026-02-10T01:40:00Z

## Objective (1 sentence)

Loom OS — a research operating system where BEAM runs on bare metal, with UEFI boot, supervised fault tolerance, and V-to-native compilation.

## Current status

### What Just Happened
- **Hackathon prep complete** (2026-02-09): Full pipeline demo working, all three tracks done.
- **JIT wired to standalone parser** — removed beam_lib OTP dependency, JIT now uses vbeam_beam_standalone.
- **Integration test 3/3 passing** — standalone parse + bare-metal interpret proven end-to-end.
- **Demo pipeline escript** — `tools/demo_pipeline.escript` showcases full pipeline in one command.
- **All scripts are pure BEAM** — zero bash/python in tools/.

### What Works
- `make compile` → 36/36 modules OK (was 33, added test + demo support)
- `make nucleus` → 5120 bytes
- `./tools/demo_pipeline.escript` → full pipeline demo (parse → interpret → JIT → nucleus)
- Integration test: standalone parse + bare-metal interpret end-to-end
- `make help` → shows all targets with descriptions
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
| `vm/interp/vbeam_beam_interp_bare.erl` | Bare-metal interpreter (856 LOC, zero OTP) |
| `vm/parser/vbeam_beam_standalone.erl` | Standalone parser (923 LOC, zero OTP) |
| `vm/jit/vbeam_beam_to_native.erl` | JIT translator (now uses standalone parser) |
| `tools/demo_pipeline.escript` | Full pipeline demo for hackathon |
| `tests/vm/test_bare_pipeline.erl` | Integration test (3/3 pass) |
| `Makefile` | Modern build system (~260 LOC) |

## What's Next

1. **Hackathon (Feb 10-16)** — submit demos, record video, polish README
2. **Self-hosting milestone** — kernel compiles V code via its own BEAM interpreter
3. **Driver framework** — supervised driver processes with hot reload
4. **V compiler integration** — wire V-to-BEAM output into the standalone pipeline

## Blockers

- None — hackathon-ready, all pipelines verified.
