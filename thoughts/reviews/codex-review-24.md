# Codex Review — Round 24

**Date**: 2026-02-10
**Findings**: 0 CRITICAL, 4 HIGH, 4 MEDIUM = 8 total (0 repeats filtered)

## HIGH
1. **native:217/88/194** — alloc_init only injected into function `<<"main">>`; if entry falls back to first function (no main), heap register uninitialized → native memory corruption.
2. **regalloc:207/334/312** — Linear scan takes first free reg regardless; call-spanning intervals can land in caller-saved regs when callee-saved unavailable → silent value clobber across calls.
3. **interp_bare:51** — load_module doesn't handle {error, Reason} from decode_instructions (R23 API change) → case_clause crash on malformed BEAM input.
4. **elf_loader:587** — Multiple relocation sections targeting same section overwrite instead of merge → silently dropped relocations, runtime corruption.

## MEDIUM
5. **jit:110/450** — lodsb emitted without clearing DF first; if Direction Flag set, string loops walk backward → wrong output, OOB reads.
6. **interp_v2:601/605, interp:567/571** — io:format called directly on guest-controlled Format/Args with no guard → badarg crashes interpreter.
7. **linuxkpi:70** — kmalloc allocates <<0:(Size*8)>> without catching allocation failures → system_limit/badarg crash instead of {error, enomem}.
8. **linuxkpi:412** — timer_fired removes by Timer only, not generation-aware → stale old timer can clear newer timer's ref, breaking cancel/rearm bookkeeping.
