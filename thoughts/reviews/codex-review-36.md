# Codex Review — Round 36

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 6 HIGH, 2 MEDIUM = 10 total

## CRITICAL
1. **arm64:59/61/75/76** — Large-frame prologue/epilogue asymmetric; epilogue restores FP/LR from wrong addr, doesn't add back FrameDelta.
2. **native.erl:653/656/667/668/672/686/687** — exit/panic builtins hardcode macOS syscall ABI; wrong on ELF/Linux. No trap fallback after syscall.

## HIGH
3. **arm64:381/387/414/327/435, regalloc:345** — print_int uses x21 (callee-saved) as scratch without save/restore; corrupts live values.
4. **native.erl:124/127, native_link:53/60/136/139** — Symbol collisions silently overwritten; data can shadow code symbols.
5. **x86_64:1615/1617, arm64:1630/1633** — Non-cycle moves in cycle path still emitted in wrong order; clobbers acyclic chains.
6. **beam_standalone:615** — Medium integer decode wrong (HighBits bor Next bsl 3); operands like 42 decode as 336.
7. **scheduler:516, irq_bridge:316** — Legacy 3-tuple IRQ still accepted. (DESIGN DECISION — skip)
8. **io_server:250** — MFA apply() before size check. (ALREADY MITIGATED with args pre-check — skip deep fix)

## MEDIUM
9. **interp_bare:105/117** — func_info-based function splitting misassigns boundary labels.
10. **interp:591** — Unknown BIFs fail open with {ok, undefined}; should return error.
