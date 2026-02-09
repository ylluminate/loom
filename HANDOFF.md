# HANDOFF

Last updated (UTC): 2026-02-09T10:00:00Z

## Objective (1 sentence)

Loom OS — a research operating system where BEAM runs on bare metal, with UEFI boot, supervised fault tolerance, and V-to-native compilation.

## Current status

### What Just Happened
- **Project separated from vbeam** (2026-02-09): All OS nucleus, kernel, native backend, and compatibility layer files migrated to this standalone repo.
- 170 files across 5 source directories (nucleus, beam_vm, kernel, compat, native)
- Standalone Makefile with `nucleus`, `qemu-test`, `compile`, `test-kernel` targets

### What Works
- `make nucleus` → builds 5KB UEFI nucleus.efi
- `make qemu-test` → boots in QEMU with serial + graphics output
- All kernel test modules have corresponding *_test.erl
- 9 native V test programs with expected output
- Standalone BEAM parser (no OTP deps) for bare-metal use

### Key Files
| File | What |
|------|------|
| `src/nucleus/vbeam_nucleus_boot.erl` | UEFI PE32+ boot (~738 LOC) |
| `src/beam_vm/vbeam_beam_interp_v2.erl` | BEAM interpreter (~530 LOC) |
| `src/beam_vm/vbeam_beam_standalone.erl` | Standalone parser (no OTP) |
| `src/kernel/vbeam_scheduler.erl` | Preemptive scheduler |
| `src/native/vbeam_native_x86_64.erl` | x86_64 backend (53 privileged insns) |
| `Makefile` | Build targets for nucleus, QEMU, tests |

## What's Next

1. **Verify `make nucleus` works from new location** — paths may need updating
2. **Update QEMU test script** — paths reference old vbeam locations
3. **Hackathon (Feb 10-16)** — demo pipeline: V → BEAM → bare metal
4. **Bare-metal BEAM interpreter** — remove remaining OTP deps from standalone parser
5. **Self-hosting milestone** — kernel compiles V code via its own BEAM interpreter

## Blockers

- Script paths may reference old `vbeam__v_on_beam` locations — need audit and update
- `make nucleus` uses relative paths that need verification in new structure
