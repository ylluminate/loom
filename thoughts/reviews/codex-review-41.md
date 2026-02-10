# Codex Review — Round 41

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 5 HIGH, 1 MEDIUM = 8 total

## Architecture (5 findings)

### CRITICAL
1. **arm64_lower:59/83/146** — Large-frame prologue: x29/x30 saved at frame top, spill offsets grow up to FrameSize-8, can overwrite saved FP/LR.

### HIGH
2. **x86_64_lower:1231/1269/1434/1483** — INT64_MIN fixup flag r10 only initialized on negative path; stale caller state corrupts last digit on positive inputs.
3. **arm64_lower:383/436/1351/1391** — INT64_MIN fixup flags (x11/x15) initialized only on negative paths but checked on all paths.
4. **x86_64_lower:1595/1690** — Cycle breaking uses SCC node order, not actual dependency order; 3+-register cycles can rotate incorrectly.
5. **native.erl:680/702, arm64_lower:307** — ARM64 Linux exit/panic set syscall number in x16 but Linux expects x8.

## Quality (3 findings)

### CRITICAL
6. **irq_bridge:225/67** — isr_ring_buffer_write only increments tail, never advances head on overflow; corrupts ring invariants.

### HIGH
7. **beam_standalone:325/327** — parse_atom_list uses compact-term decoding for atom names instead of raw length-prefixed entries.
8. **elf_loader:435/483** — SHT_NOBITS sections materialized before cumulative cap enforced; crafted ELF forces large allocs.

### MEDIUM
9. **beam_parser:348, beam_standalone:417** — Malformed LitT payloads treated as empty instead of errors.
