# Codex Review — Round 43

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 7 HIGH = 9 total

## Architecture (5 findings)

### CRITICAL
1. **regalloc:702/733, x86_lower:442, arm64_lower:283** — call_indirect not modeled as use in spill analysis; indirect branch targets get post-store instead of pre-load.
2. **regalloc:58/94/353, x86_lower:593** — Params pre-assigned to ABI regs, but clobbering pseudo-ops not treated as call barriers; live params silently corrupted.

### HIGH
3. **x86_lower:654/670/724/1446** — x86-64 hardcodes Linux syscall numbers; wrong on Mach-O.
4. **arm64_lower:208/215, x86_lower:287** — ARM64 sdiv/srem has no divide-by-zero guard; silent semantic divergence from x86 trap behavior.
5. **x86_lower:1041/1058** — map_put traps with UD2 at capacity instead of growing; deterministic crash on valid workloads.

## Quality (5 findings)

### CRITICAL
6. **scheduler:267/507/527, irq_bridge:322/331** — Timer IRQ delivery path inconsistent: scheduler never registers as handler, bridge sends 3-tuple but scheduler expects 4-tuple token. Can drop all timer ticks.

### HIGH
7. **irq_bridge:131/268/407** — Any process can claim IRQ ownership; no privilege/capability check on register_handler.
8. **linux_syscall:39/47/129** — Syscall dispatch lets non-badarg exceptions escape; can crash kernel paths.
9. **beam_standalone:320/335/436, beam_parser:259/267/275/290/366** — BEAM parsing fails open on truncation; partial/empty data instead of errors.
10. **linuxkpi:56/575/590/598** — Timer ETS table is public; any process can tamper with timers.
