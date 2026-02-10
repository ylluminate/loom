# Codex Review — Round 21

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 4 HIGH, 3 MEDIUM, 1 LOW = 9 total (1 repeat filtered)

## CRITICAL
1. **x86_64:1088** — `int_to_str` reuses `r15` (heap register) as INT64_MIN flag, corrupting heap pointer.

## HIGH
2. **arm64:1193** — `encode_cmp_rrr(x19, x9)` calls nonexistent function; should be `encode_cmp_rr`.
3. **macho/native:239** — Mach-O ARM64 data_base hardcodes 4KB pages; emitter uses 16KB. Layout mismatch.
4. **jit:438** — serial output path reads stale RSI with lodsb, no length bound on put_string path.
5. **elf_loader:294** — catch/case pattern mismatch: `error(no_init_symbol)` produces `{'EXIT',{no_init_symbol,...}}` not `{'EXIT',{error,no_init_symbol,...}}`.

## MEDIUM
6. **linuxkpi:53/513** — Timer ETS table `protected`; non-owner processes crash on `ets:insert`.
7. **syscall:25/323** — Tracking ETS also `protected`; non-owner inserts fail, dedup never works.
8. **link:325** — `patch_arm64_cond_branch19` missing bounds check (sibling has it).

## LOW
9. **paging_test:278** — Misalignment test commented out.

## REPEAT (filtered)
- Scheduler IRQ auth — DOCUMENTED as intentional for testing/bootstrap (R14).
