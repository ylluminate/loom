# Codex Review — Round 29

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 6 HIGH, 2 MEDIUM, 1 LOW = 10 total (0 repeats filtered)

## CRITICAL
1. **x86_64_lower:580/584/820/824/1238/1266, arm64_lower:557/562/840/844/1042/1046/1348/1370, native:666/691/816** — Multiple paths do raw bump-pointer heap math without checking heap_end or wrap detection, bypassing alloc safety.

## HIGH
2. **alloc:198/200/227/229** — emit_alloc_reg only checks new_ptr <= heap_end without detecting arithmetic wraparound; wrapped pointer passes bound check.
3. **regalloc:606/626/632/558/596** — Unknown instruction analysis marks {all_vregs, all_vregs} as defs/uses; post-call spill stores from clobbered scratch regs write garbage to spill slots.
4. **regalloc:570/594/655/681, x86_64_lower:1482/1483, arm64_lower:1499/1500** — rewrite_operand_multi doesn't recurse into nested lists/tuples; method_call args keep unreplaced {vreg,N}, lowerers fallback to {imm,0}.
5. **irq_bridge:253/258/130** — First-time IRQ registration has no authorization; any process can claim timer IRQ 32.
6. **interp_bare:51/53** — load_module executes partial instruction streams on decode failure (fails open on malformed BEAM).
7. **page_alloc:50/53** — Allocator init has no upper bound on TotalMemoryBytes; huge value crashes bitmap construction.

## MEDIUM
8. **paging:93/97** — load_cr3_code accepts non-4KB-aligned addresses; misaligned CR3 causes triple-fault.
9. **io_server:214/215/218** — Output capped at 64KB only after full materialization; large put_chars causes memory spike.

## LOW
10. **standalone:46/50/73** — parse_binary has no size cap (parse_file enforces 100MB); direct callers bypass policy.
