# Codex Architecture Review — Round 11

**Date**: 2026-02-08
**Scope**: Full codebase (arch/, vm/, kernel/, compat/)
**Findings**: 5 CRITICAL, 8 HIGH, 4 MEDIUM = 17 total

## CRITICAL

1. **x86_64:390** — RET teardown resets rsp to rbp before restoring callee-saved registers, so pops read saved rbp/ret metadata and corrupt the frame. Fix: Pass FrameSize into RET lowering and emit add rsp,FrameSize then pop callee-saved in reverse then pop rbp and ret.

2. **arm64:59** — Large-frame prologue sets x29 to the frame bottom while spill/callee slots are addressed as x29+offset, so large spill offsets overlap saved x29/x30. Fix: Keep x29 anchored at the FP/LR save slot (or add FrameDelta when setting x29) and keep spill/callee offsets relative to that anchor.

3. **regalloc:524** — Spill rewrite hardcodes scratch register r11 for all targets, generating invalid arm64 operands when spills occur. Fix: Choose scratch register by target (r11 on x86_64, x15 on arm64) and thread target into spill rewrite.

4. **alloc:112** — x86 allocation helpers call nonexistent encoder functions encode_jns/1 encode_jle/1 and encode_int3/0, causing runtime undef when emit_alloc* is used. Fix: Implement/export those encoders or replace calls with existing encode_jcc_rel32 and encode_int forms.

5. **alloc:153** — arm64 allocation helpers call nonexistent encode_brk/1, so emit_alloc* crashes at codegen time. Fix: Add encode_brk/1 to vbeam_native_arm64 or replace with existing trap instruction sequence.

## HIGH

6. **x86_64:1434** — Cycle-breaking for argument moves uses maps:get(From,MoveMap) and emits self-moves for multi-node cycles, misrouting call arguments. Fix: Implement true cycle rotation with one temp register and dependency-ordered moves.

7. **x86_64:1452** — Method-call stack argument loads ignore saved-callee offset, so spilled arguments are read from wrong frame slots when callee-saved registers are pushed. Fix: Thread UsedCalleeSaved or frame metadata into emit_move_x86 and include saved-callee bytes in stack-slot address calculation.

8. **arm64:1479** — ARM64 cycle breaker emits other cycle moves in arbitrary order and then restores temp to source, clobbering values in 3+ register permutations. Fix: Emit deterministic rotate sequence.

9. **regalloc:299** — method_call is matched as arity-3 in call detection, so real method_call instructions are missed and live caller-saved values can be clobbered across calls. Fix: Match actual arity-5 method_call tuple in is_call_instruction.

10. **regalloc:526** — All spilled uses are reloaded into the same scratch register before one instruction, so multi-spill ops use only the last loaded value. Fix: Allocate distinct temporaries per spilled operand or lower into sequenced two-address operations.

11. **regalloc:558** — Unknown opcodes default to no uses/defs, so spilled operands in pseudo-ops are rewritten to scratch without required reload/store insertion. Fix: Add explicit use/def analysis for all IR opcodes or conservative recursive fallback.

12. **alloc:113** — Allocator emits relocation type x86_rel32 but linker resolve logic only handles rel32, causing function_clause during relocation resolution. Fix: Emit rel32 consistently or add x86_rel32 handling.

13. **jit:406** — translate_put_string returns a null pointer placeholder, and subsequent string output paths dereference it. Fix: Emit real string data references and load a valid RIP-relative pointer.

## MEDIUM

14. **x86_64:1048** — int_to_str initializes index at 47 but computes len as 48-index, returning one extra garbage byte. Fix: Initialize index at 48 or adjust final length formula.

15. **x86_64:1074** — int_to_str uses xor rdx,rdx before idiv; when negation overflows on INT64_MIN the dividend is not sign-extended and division semantics break. Fix: Use cqo before idiv and add explicit INT64_MIN handling.

16. **arm64:1165** — arm64 int_to_str has the same 47/48 index math bug. Fix: Start from 48 or recompute length.

17. **x86_64:898** — map_put silently returns the original map when capacity is full, dropping inserts without error. Fix: Implement growth/reallocation or return failure/trap.
