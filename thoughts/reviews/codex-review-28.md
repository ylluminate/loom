# Codex Review — Round 28

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 7 HIGH, 1 MEDIUM, 1 LOW = 10 total (0 repeats filtered)

## CRITICAL
1. **regalloc:79/342/344/432/441, alloc:136/164** — Heap-end registers (r14/x27) not reserved in regalloc; user vregs can overwrite them, breaking allocation bounds checks.

## HIGH
2. **arm64:93/101/113/136/142** — emit_stack_load/store returns bare binary for small offsets, list for large; lower_body does Parts ++ ... causing badarg.
3. **regalloc:575/580/581** — Spill rewrite lists:zip with fixed 2 scratch regs crashes on 1-spill instructions (unequal lengths).
4. **native:987/997/999** — int_to_str_builtin never moves result to ABI return register (x0/rax); returns wrong value.
5. **native:233/236/244/197** — inject_into_entry only injects into main if it's first in function order; if main is later, alloc init goes to wrong function.
6. **syscall:38/42-44** — dispatch/2 blanket try/catch swallows ALL exceptions including exit/throw; hides real failures.
7. **page_alloc:151/198/305** — mark_reserved accepts negative addresses; negative page indices corrupt free_count accounting.
8. **interp_bare:483** — gc_bif malformed operands crash via badmatch in split_at_last; no error handling.

## MEDIUM
9. **linuxkpi:149** — printk fallback uses ~s on non-string Fmt, crashes again. Needs ~tp or double try/catch.

## LOW
10. **x86_64_enc:864/868** — encode_mov_from_seg omits REX.W for low registers; upper 48 bits stale.
