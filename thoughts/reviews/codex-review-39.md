# Codex Review — Round 39

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 8 HIGH, 3 MEDIUM = 12 total

## CRITICAL
1. **alloc:189/193/271/280/284** — x86 heap alloc calls encode_jb/encode_jbe which don't exist; undef crash during compilation.

## HIGH
2. **alloc:119, x86_64:273** — macOS heap init uses jcc(nc,...) but no `nc` in cond_code; function_clause crash.
3. **native.erl:678/701, arm64:1506** — exit/panic emit `trap` IR but ARM64 has no trap lowering; unhandled_arm64_instruction.
4. **regalloc:361/349/355/446/457/601** — Call-aware pool reintroduces r10/x14/x15 scratch regs excluded by available_regs.
5. **native_link:54/71/153/157** — Symbol collisions only warned, still overwritten; data can shadow function.
6. **interp:120/327/345** — build_label_map doesn't handle 2-tuple decode results; case_clause crash.
7. **interp:85/135/152, parser:243** — Parser atoms are binaries but function lookup compares atoms; resolution fails.
8. **page_alloc:287/290** — Wrapped contiguous search skips valid positions below Hint for Count>1.
9. **irq_bridge:210** — ISR reads IRQ from [rsp+56] but pushes changed stack layout; wrong IRQ number.

## MEDIUM
10. **x86_64:242/250/401, x86_64:347/366/483** — imm32 truncation for out-of-range immediates.
11. **beam_standalone:771/529** — Unknown extended operand accepted as {{unknown,extended},...}; fail-open.
12. **gdt_idt:403/412/421** — Exception char in AL overwritten by UART poll; wrong byte emitted.
