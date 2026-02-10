# Codex Review — Round 33

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 10 HIGH = 13 total (0 repeats filtered)

## CRITICAL
1. **x86_64:1170/1176/1212/1263, alloc:241** — int_to_str repurposes r14 as sign flag, but r14 is heap_end; corrupts allocation bounds checks.
2. **io_server:192/194/259/263** — put_chars path materializes iodata with no size precheck; large request kills IO server.
3. **io_server:148/151/155/157** — direct_write converts arbitrary input with no size cap; bypasses guarded path.

## HIGH
4. **x86_64:1466/1473, native:226** — print_float injects raw IR tuple into lowered parts; crashes assembly.
5. **regalloc:652/658/660/661** — Operand analysis has opcode/arity mismatches: str_concat vs string_concat, wrong arity for map_new and map_put; defs lost.
6. **regalloc:323/331/487/507/649, x86_64:1466, arm64:1226** — print_float not treated as call-clobbering in scratch exclusion.
7. **arm64:716/755, arm64_enc:312/825** — ARM64 imm-index array paths use encode_cmp_imm without range check; Idx >= 4096 crashes compilation.
8. **beam_to_native:280/282/342/378** — Standalone-decoded external calls not normalized; call_ext_only not handled, translation fails.
9. **beam_to_native:430/436** — translate_move only supports atom moves to {x,0}; common moves fail.
10. **elf_loader:572/628** — Relocation sections selected by name not type; nonstandard names skipped.
11. **elf_loader:29/567/613** — Only RELA parsing implemented; REL relocations unsupported.
12. **elf_loader:51/794/800** — SHN_COMMON symbols not handled; trigger missing_section_address.
13. **beam_parser:216/217/221/223** — Atom table count unbounded in parser v1; memory exhaustion.
