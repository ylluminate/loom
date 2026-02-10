# Codex Review — Round 42

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 6 HIGH, 1 MEDIUM = 10 total

## Architecture (5 findings)

### CRITICAL
1. **arm64_lower:33/57/168/174** — Large-frame FP/LR at FrameSize-16, spill slots from x29+16 overlap FP/LR save area for large frames.

### HIGH
2. **arm64_lower:62/67/90/95, arm64:14/630/653** — Large-frame uses encode_stp/4, encode_ldp/4 which don't exist (only _pre/_post variants).
3. **regalloc:414/440/446/110/575/377** — Call-spanning params in caller-saved regs removed from scan set; never spilled, clobbered on calls.
4. **interp_v2:207/214, interp_bare:605/620** — Call resolution ignores InstrIndex, forces pc=0; breaks non-zero label entries.

### MEDIUM
5. **regalloc:633/635** — Spill rewriting aborts on >2 spilled vregs with too_many_spills_in_instruction.

## Quality (5 findings)

### CRITICAL
6. **interp:86/184/188, parser:231** — binary_to_atom/2 on untrusted input; atoms not GC'd, exhausts atom table.
7. **irq_bridge:117/319/323/345, scheduler:512/522** — IRQs never ack'd; pending_counts only grows, timer IRQs eventually dropped permanently.

### HIGH
8. **scheduler:506/520/521/522** — Legacy 3-tuple IRQ handler bypasses token auth; any process can spoof timer ticks.
9. **irq_bridge:65/67/77/225/230/232** — ISR ring only increments tail, never advances head on overflow; ring invariants violated.
10. **elf_loader:932/936** — SHN_COMMON symbols resolved to address 0; relocations against COMMON data point to null.
