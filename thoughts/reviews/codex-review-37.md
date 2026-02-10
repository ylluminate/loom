# Codex Review — Round 37

**Date**: 2026-02-10
**Findings**: 0 CRITICAL, 5 HIGH, 4 MEDIUM = 9 total (first round with no CRITICALs!)

## HIGH
1. **arm64_enc:166, alloc:157/214/222** — Allocator emits b.cond `lo` but encoder lacks `lo` alias; function_clause crash.
2. **alloc:96/111/120** — x86_64 alloc init always uses Linux syscall ABI; wrong for Mach-O/PE.
3. **native.erl:657/659/679/681** — exit/panic matches `elf` but format atom is `elf64`; falls through to macOS path.
4. **elf_loader:693/765** — REL addend still hardcoded 0 (repeat finding — needs real implicit addend).
5. **interp:60** — init_proc does maps:get('Code', Chunks) with no guard; badkey crash on malformed modules.

## MEDIUM
6. **native_ir:245, native.erl:177/1067** — Data validation missing; alignment=0 → badarith.
7. **regalloc:422, arm64:1494, x86_64:1494** — store_spill not lowered by backends; arm64 hard-fails, x86_64 traps.
8. **scheduler:337** — spawn_process double-wraps allocator errors: {error, {error, Reason}}.
9. **syscall:42** — function_clause maps to ENOSYS(38) instead of EINVAL(22).
