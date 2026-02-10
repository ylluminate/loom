# Codex Review — Round 34

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 7 HIGH, 3 MEDIUM = 12 total (0 repeats filtered)

## CRITICAL
1. **x86_64:807/881, arm64:830/897** — array_append never enforces ElemSize==8; writes full 64-bit with smaller elements → heap corruption.
2. **beam_to_native:467/476/499** — put_string never initializes RSI before lodsb; generated code reads arbitrary memory.

## HIGH
3. **native.erl:134/303, macho:55/365** — Mach-O relocation uses x86_64 page math on arm64; bad data base → corrupt relocations.
4. **regalloc:655/663** — str_len typo (should be string_len); defs lost for mismatched opcodes.
5. **regalloc:98/269/118/413** — Spilled parameters have no prologue store; reloads read uninitialized stack slots.
6. **scheduler:498/507/509** — Timer IRQ messages unauthenticated; any process can spoof ticks.
7. **linuxkpi:54/573/581/596** — Timer ETS table is protected; non-owner writes crash mod_timer/del_timer.
8. **io_server:241/244** — MFA put_chars runs apply() before size check; large formatter output bypasses cap.
9. **beam_parser:216/221** — Malformed atom chunks raise error() instead of returning {error,...}; crashes callers.

## MEDIUM
10. **x86_64:1412, arm64:378** — print_int INT64_MIN negation overflow; incorrect output for that value.
11. **beam_standalone:530/532/562/582** — Error tuples matched by overly broad patterns; error reasons lost.
12. **elf_loader:667/679** — SHT_REL hard-codes addend=0 instead of deriving from target contents.
