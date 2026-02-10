# Codex Review — Round 22

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 5 HIGH, 5 MEDIUM, 1 LOW = 13 total (3 repeats filtered)

## CRITICAL
1. **x86_64:647** — `array_get`/`array_set` emit no bounds checks; out-of-range index corrupts heap.
2. **arm64:648** — Same: `array_get`/`array_set` no bounds checks.

## HIGH
3. **x86_64:1403** — `method_call` argument moves not parallel-copy safe (register clobbering).
4. **arm64:1424** — Same: argument shuffling not dependency-safe.
5. **native:583** — Builtin `exit`/`panic` syscall not ABI/format-aware per target.
6. **io_server:197** — `length(Args)` crashes on non-list MFA args.
7. **elf_loader:224/460** — SHT_NOBITS (.bss) sections dropped (nobits → empty binary).

## MEDIUM
8. **alloc:91** — ARM64 heap init `encode_add_imm` with >4096 immediate (exceeds imm12).
9. **x86_64:1092** — `int_to_str` hardcoded labels without unique suffix (collide on multi-function).
10. **arm64:1180** — `int_to_str` sign flag read from potentially clobbered register.
11. **scheduler:356** — `kill_process` doesn't clear `current_pid`.
12. **syscall:233** — `ARCH_GET_FS`/`ARCH_GET_GS` cap check blocks existing key updates.

## LOW
13. **linuxkpi:352** — `mod_timer` stale message race (timer fires between cancel and re-arm).

## REPEATS (filtered)
- Scheduler IRQ auth — DOCUMENTED as intentional (R14, R16, R21).
- `test_elf_loader_bounds` simulates instead of real testing — test quality, not a bug.
- Scheduler test coverage gap — test quality, not a bug.
