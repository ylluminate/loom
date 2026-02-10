# Codex Review — Round 35

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 3 HIGH, 3 MEDIUM = 9 total

## CRITICAL
1. **x86_64:432/70** — ret with no callee-saved regs doesn't restore rsp from spill frame; pops from spill area.
2. **arm64:45/58/81/135** — Large-frame prologue sets x29 before full stack alloc; spill accesses above frame → corruption.
3. **beam_to_native:344/360/468/476** — call_ext for io:format/erlang:display emits serial_output with uninitialized RSI.

## HIGH
4. **x86_64:1548, arm64:1565** — Argument moves emitted in input order, not dependency-safe; acyclic chains clobber.
5. **scheduler:516, irq_bridge:316** — Legacy 3-tuple IRQ still accepted; bypassable auth. (DESIGN DECISION — bare-metal context)
6. **beam_standalone:723/566** — decode_extended matches {error,Reason} as {Items,Rest3}; crafted bytecode crashes decoding.

## MEDIUM
7. **regalloc:588/603/604/609** — Spill counting double-counts use+def vregs; same vreg can map to different scratch regs.
8. **io_server:250/253** — apply() before size check in MFA put_chars; io_lib:format can generate huge iolists.
9. **elf_loader:680/685** — REL addend still hardcoded 0; incorrect for non-zero addends.
