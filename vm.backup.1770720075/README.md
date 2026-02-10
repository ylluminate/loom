# vm/ — BEAM Virtual Machine

BEAM bytecode execution layer for Loom OS.

## Subsystems

| Directory | Module(s) | Purpose |
|-----------|-----------|---------|
| `interp/` | `vbeam_beam_interp`, `_v2`, `_bare` | BEAM interpreters: v1 (minimal), v2 (beam_disasm-based, 30+ opcodes), bare (no OTP deps) |
| `parser/` | `vbeam_beam_parser`, `vbeam_beam_standalone` | BEAM file parsers: standard (IFF chunks) and standalone (zero OTP dependencies, for bare-metal) |
| `jit/` | `vbeam_beam_to_native` | BEAM bytecode → x86_64 machine code translator (361 LOC) |

## Key Design Decision

The standalone parser (`vbeam_beam_standalone`) has **zero OTP dependencies** — it can run on bare metal without `beam_disasm`, `beam_lib`, or any other standard library module. This is essential for the self-hosting boot path.

## Tests

VM tests live in `tests/vm/`. Run with:

```bash
make test-vm
```
