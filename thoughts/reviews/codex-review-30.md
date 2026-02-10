# Codex Review — Round 30

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 6 HIGH, 3 MEDIUM = 13 total (0 repeats filtered)

## CRITICAL
1. **native:74/88, regalloc:462, native:666/693/815/819** — needs_heap detector only matches small opcode whitelist; injected builtins (string_to_upper, get_raw_line) mutate heap register directly but aren't detected; alloc_init skipped → uninitialized heap pointer.
2. **heap:154/159/160** — alloc/3 has no guard for NumWords > 0; negative values reduce `used`, allowing overlapping allocations (heap corruption).

## HIGH
3. **regalloc:400/403/223/588/410** — When call-spanning params outnumber callee-saved regs, reassign_params deletes param mapping; those params get spilled to uninitialized stack slots.
4. **alloc:198/206/210/239/248/252** — Overflow/wrap/bounds checks use signed comparisons (lt/le, jl/jle) for unsigned pointer arithmetic; high-bit values bypass checks.
5. **paging:95/100** — load_cr3_code only checks 4KB alignment, not reserved high bits; invalid bits trigger #GP/triple-fault.
6. **paging:139/237** — `ps` flag accepted at non-pd levels; sets reserved bits in PML4/PDPT entries → CPU fault.
7. **standalone:99/100/143/442/443** — Chunk parser errors not propagated; malformed Code chunk returns binary; build_result does maps:get on binary → badmap crash.
8. **elf_loader:463/465/428** — .bss (SHT_NOBITS) eagerly materialized per-section with no total cap across sections; many sections → OOM.

## MEDIUM
9. **alloc:227/234/238, x86_64_lower:1232** — x86 dynamic allocator temp can alias SizeReg; self-compare disables overflow check.
10. **alloc:149/169** — emit_alloc constant-size path has no wraparound check (new_ptr < old_ptr) before bounds check.
11. **gdt_idt:438/441** — encode_mov_imm64 uses signed 64-bit; canonical addresses can badarg during encoding.
12. **linuxkpi:53/558/596/597** — Timer table is public ETS; any process can poison it; prune_dead_timers lacks is_pid guard → malformed keys crash pruning.
13. **interp_bare:772/328/345/355** — unwrap_int silently converts invalid operands to 0; bypasses stack validation, corrupts interpreter frame.
