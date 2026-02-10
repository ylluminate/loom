# Codex Review — Round 40

**Date**: 2026-02-10
**Findings**: 1 CRITICAL, 6 HIGH, 3 MEDIUM = 10 total

## Architecture (5 findings)

### HIGH
1. **regalloc:223/269** — Spill-at-interval can reassign call-spanning intervals to caller-saved regs; calls clobber live values.
2. **regalloc:684, x86_64_lower:931** — Unknown-opcode fallback marks operands as uses-only; defining ops (field_get, struct_new, fadd) lose computed results.
3. **regalloc:88/98, x86_64_lower:853** — Preassigned parameter vregs can remain in scratch regs that pseudo-op lowerings clobber.
4. **x86_64_lower:1614/1663** — 3+-register cycle resolver has incorrect lowlink propagation and rotation order.

### MEDIUM
5. **native.erl:93/124** — BSS section dropped from pipeline; uninitialized globals not laid out.

## Quality (5 findings)

### CRITICAL
6. **interp:121/133** — build_label_map doesn't handle 5-tuple instructions (func_info); case_clause crash.

### HIGH
7. **beam_parser:227/232** — AtU8 atom count treated as unsigned-only; valid modern BEAM tables rejected.
8. **interp:389/412/426** — decode_opcode uses wrong opcode numbers for allocate/test_heap/call_ext.
9. **interp_bare:216/360/550** — call_ext_last not handled; modules with it hit unknown_instruction.

### MEDIUM
10. **interp_bare:368/864** — Oversized stack allocations silently ignored; execution continues broken.
11. **elf_loader:522/594/631** — Symbol/reloc counts use integer division without remainder check; truncated tables accepted.
