# Codex Review — Round 32

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 10 HIGH, 1 MEDIUM = 13 total (0 repeats filtered)

## CRITICAL
1. **regalloc:550/563/615/641** — Spill rewrite depends on Defs from analyze_inst_operands, but most opcodes fall to uses-only fallback; spilled destination vregs never stored back → silent value corruption under register pressure.
2. **alloc:79/86/88/117/120/122** — Allocator init failure paths call exit syscall but don't terminate control flow; if syscall returns, execution falls through to success label → heap corruption.

## HIGH
3. **x86_64:1003/1005/1021/1026** — x86_64 map_put has no growth path; once len >= cap (initial 8), emits UD2 → deterministic crash on 9th insertion.
4. **ir:177/283, x86_64:1462/1466** — IR accepts print_float but x86_64 lowering has no clause → falls to catch-all UD2.
5. **native:135/137, link:242/245/314/342** — Relocation patch failures throw error() with no protection in do_compile → compiler process crash.
6. **scheduler:395** — send_message calls external_size with no try/catch; system_limit crashes scheduler gen_server.
7. **parser:99/109, interp:59** — Malformed Code chunks returned as raw binary; interpreter maps:get on binary → badmap crash.
8. **standalone:966/1149/541** — Unknown opcodes treated as valid with arity 0; desynchronizes bytecode decoding.
9. **standalone:312/314** — Negative atom count accepted via abs(); bypasses positive-count cap.
10. **elf_loader:705/767** — R_X86_64_64 relocation value not masked to 64 bits; overflow → badarg.
11. **page_alloc:99/103/166/172** — Address APIs lack integer guards; non-integer → badarith crash.
12. **linuxkpi:352/355/358/360** — schedule_work/queue_work report success but do nothing → driver deadlocks.
13. **interp:34, interp_v2:57** — Execute APIs call length(Args) without is_list guard → badarg on malformed input.
