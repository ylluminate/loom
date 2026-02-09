# AI Instructions (Canonical)

Project: **loom**
Archetype: **dev**

This file (`AI.md`) is the **single source of truth** for how any AI agent should operate in this project.

## What is Loom OS?

A research operating system where BEAM *is* the kernel. V programs compile to BEAM bytecode or native x86_64. Everything runs as supervised Erlang processes. Drivers crash and restart in <1ms.

## Key Architecture

```
loom/
├── boot/           UEFI PE32+ boot, GOP framebuffer, VGA fonts
├── kernel/         Core kernel: boot sequence, memory, scheduler, I/O, GDT/IDT
│   ├── boot/       Boot sequence orchestration
│   ├── mm/         Page allocator, paging, heap
│   ├── sched/      Preemptive scheduler
│   ├── io/         Serial/framebuffer, IRQ bridge
│   └── arch/       GDT and IDT setup
├── vm/             BEAM virtual machine
│   ├── interp/     Interpreters (v1, v2, bare)
│   ├── parser/     BEAM file parsers (standard + standalone/no-OTP)
│   └── jit/        BEAM bytecode → x86_64 translator
├── arch/           Native code generation
│   ├── x86_64/     x86_64 encoder + lowering
│   ├── arm64/      ARM64 encoder + lowering
│   ├── ir/         IR, register allocator, allocator
│   ├── link/       Linker + native entry point
│   └── formats/    PE32+, ELF64, Mach-O emitters
├── compat/         Linux compatibility
│   ├── syscall/    ~450 Linux syscalls mapped
│   ├── elf/        ELF64 .ko loader
│   └── kpi/        66 LinuxKPI shims
├── tests/          All tests (kernel/, vm/, native/, data/)
└── tools/          Build scripts, QEMU helpers, test runners
```

## Build Commands

```bash
make help             # Show all targets with descriptions
make all              # Build everything (compile + nucleus)
make nucleus          # Build 5KB UEFI bootable nucleus.efi
make qemu-test        # Boot in QEMU, verify serial output
make compile          # Compile all Erlang modules
make boot-compile     # Compile only boot/ modules
make kernel-compile   # Compile only kernel/ modules
make vm-compile       # Compile only vm/ modules
make arch-compile     # Compile only arch/ modules
make compat-compile   # Compile only compat/ modules
make test             # Run all tests
make test-kernel      # Run kernel tests
make test-vm          # Run VM tests
make check            # Syntax-check all .erl files
make info             # Show module counts and build status
make clean            # Clean build artifacts
make distclean        # Clean everything including generated files
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
