# arch/ — Architecture Backends

Native code generation for x86_64 and ARM64.

## Subsystems

| Directory | Module(s) | Purpose |
|-----------|-----------|---------|
| `x86_64/` | `vbeam_native_x86_64`, `_lower_x86_64` | x86_64 instruction encoder (raw bytes, REX.W) + 53 privileged instructions (CLI/STI/HLT/CPUID/RDTSC/MOV CR/IRETQ). Lowering from IR to machine code. |
| `arm64/` | `vbeam_native_arm64`, `_lower_arm64` | ARM64/AArch64 instruction encoder (32-bit fixed-width). 68/68 V examples compile. |
| `ir/` | `vbeam_native_ir`, `_regalloc`, `_alloc` | Intermediate representation, linear scan register allocator, bump allocator |
| `link/` | `vbeam_native`, `_link` | Native code entry point (escript), internal linker (symbol resolution + relocation patching) |
| `formats/` | `vbeam_native_pe`, `_elf`, `_macho` | Object file emitters: PE32+ (UEFI/Windows), ELF64 (Linux), Mach-O 64-bit (macOS) |

## Pipeline

```
V AST → IR (ir/) → Register Allocation (ir/) → Lowering (x86_64/ or arm64/)
  → Machine Code → Object Format (formats/) → Linking (link/) → Executable
```
