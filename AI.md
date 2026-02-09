# AI Instructions (Canonical)

Project: **loom**
Archetype: **dev**

This file (`AI.md`) is the **single source of truth** for how any AI agent should operate in this project.

## What is Loom OS?

A research operating system where BEAM *is* the kernel. V programs compile to BEAM bytecode or native x86_64. Everything runs as supervised Erlang processes. Drivers crash and restart in <1ms.

## Key Architecture

- `src/nucleus/` — UEFI PE32+ boot, GOP framebuffer, VGA fonts
- `src/beam_vm/` — BEAM interpreter + standalone parser (no OTP deps)
- `src/kernel/` — GDT/IDT, paging, heap, scheduler, I/O, IRQ, BEAM-to-native JIT
- `src/compat/` — Linux syscalls, ELF .ko loader, LinuxKPI shims
- `src/native/` — x86_64 + ARM64 backends, IR, regalloc, PE/ELF/Mach-O emitters

## Build Commands

```bash
make nucleus          # Build 5KB UEFI bootable nucleus.efi
make qemu-test        # Boot in QEMU, verify serial output
make compile          # Compile all Erlang modules
make test-kernel      # Run kernel module tests
make check-erl        # Syntax-check all .erl files
make clean            # Clean build artifacts
```

## Operating rules

1. **Truth over speed.** If uncertain, ask or propose a test.
2. **Small steps.** Prefer small, reviewable changes.
3. **Plan-first by default.** If there is no `PLAN.md` (or it's stale), update it before implementing.
4. **Continuity is mandatory.**
   - Update `LEDGER.md` when decisions are made or state changes.
   - Update `HANDOFF.md` whenever you stop, switch tools, or hit context limits.
5. **Write everything down.** The filesystem is our long-term memory.

## Where things go

- `PLAN.md` — current plan + acceptance criteria
- `LEDGER.md` — durable project state, decisions, constraints
- `HANDOFF.md` — tactical baton pass: what changed, what's next
- `thoughts/sketches/` — architecture diagrams and design notes
- `docs/` — detailed technical documentation

## Relationship to vbeam

The V-to-BEAM compiler lives in `vbeam__v_on_beam`. The OS kernel, native backends, and boot infrastructure live here. This project was separated from vbeam for clarity on 2026-02-09.

## Safety

- Do not write secrets into the repo.
- Kernel code must be especially careful about memory safety.
- PE32+/ELF emitters deal in raw bytes — verify offsets against specs.
