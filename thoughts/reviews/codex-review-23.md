# Codex Review — Round 23

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 6 HIGH, 3 MEDIUM = 12 total (0 repeats filtered)

## CRITICAL
1. **regalloc:312/native:638/x86:403/arm64:86** — Heap pointer (r15/x28) saved+restored as callee-saved in prologue/epilogue; builtins that bump heap lose their updates on return (heap corruption/reuse).
2. **arm64:656/681/709/736** — Array bounds error path uses wrong syscall ABI: hardcodes x16=1 + svc #0 (macOS convention), but Linux ARM64 needs x8 + svc #0. If syscall returns instead of exiting, falls through to OOB access.
3. **elf_loader:462** — `extract_section_data` allocates `<<0:(Size*8)>>` for SHT_NOBITS with ELF-controlled Size and no cap. Crafted .bss can OOM the VM.

## HIGH
4. **regalloc:68/300/x86:607** — Pseudo-ops (print_str, int_to_str) clobber caller-saved regs but aren't treated as call barriers; live params in r8+ silently corrupted.
5. **native:72/135/201, elf:91** — `.bss` sections dropped by compile/link pipeline. Never symbolized, ELF hardcodes BssSize=0. Breaks uninitialized globals.
6. **standalone:510, jit:140** — Standalone BEAM decoder fails open: returns partial instruction stream on error instead of {error, Reason}. JIT compiles truncated stream into unsafe native code.
7. **linuxkpi:356** — mod_timer cancels OldTRef directly but stored refs are {TRef, Gen} tuples (from R22 fix). cancel_timer/1 badarg on re-arm.
8. **io_server:148** — handle_cast({direct_write, Data}) calls unicode:characters_to_binary without protection. Invalid iodata crashes the registered IO server.
9. **interp:97/388** — build_label_map assumes 3-tuple decode results, but unknown opcodes decode as 2-tuple {unknown_opcode, NextPC}. case_clause crash during init.

## MEDIUM
10. **x86:1351, arm64:356** — print_int mishandles INT64_MIN: plain neg overflows and remains negative; subsequent digit extraction emits wrong chars.
11. **syscall:38** — dispatch/2 has only guarded clauses; malformed call shape hits function_clause instead of {error, EINVAL}.
12. **io_server:115/381** — max_log_size clamped as bytes in init but enforced as entry count in output_log. Unit mismatch allows more memory than intended.
