# Codex Review — Round 38

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 5 HIGH, 4 MEDIUM = 10 total

## CRITICAL
1. **alloc:111/117/118** — x86_64 mmap uses Linux flags (0x22) on macOS; error check uses jns but macOS uses CF; can treat error as valid heap ptr.

## HIGH
2. **x86_64:19/632/646/699/1411** — x86 lowering hardcodes Linux syscalls in pseudo-ops; doesn't thread format.
3. **regalloc:447/456/598/599** — Spill scratch regs (r10, x14/x15) are allocatable; can clobber live vregs.
4. **x86_64:1619/1624** — Cycle moves emitted before non-cycle; non-cycle reads see rotated values.
5. **elf_loader:767/770** — RELA with addend=0 triggers implicit addend read; regression from R37 fix.
6. **interp:64/66/31** — init_proc throws hard errors; execute/4 doesn't catch; malformed input crashes caller.

## MEDIUM
7. **arm64:398/417/431** — print_int x10 used as both INT64_MIN flag and quotient; flag clobbered.
8. **beam_parser:193/200/207** — catch binary_to_term returns {'EXIT',...} accepted as valid term.
9. **beam_to_native:294/344/387** — Local call opcodes not normalized/translated; fall to unknown-opcode.
10. **boot_sequence:33/54** — boot_code spec says binary() but can return {error,...}.
