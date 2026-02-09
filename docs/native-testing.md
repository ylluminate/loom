# Native Backend Testing Guide

This document covers the testing infrastructure for vbeam's native code generation backend.
The native backend compiles V source code to bare-metal executables (ARM64 Mach-O, x86_64 ELF,
x86_64 PE/UEFI) without the BEAM VM.

## Overview

The native test pipeline:

```
V Source (.v)
    |
V Compiler (VBEAM_TARGET=<arch> v -b beam)
    |
Native IR (.vbeam_native.ir)
    |
vbeam_native.erl (Erlang-based assembler/linker)
    |
Native Binary (Mach-O / ELF / PE)
    |
Execute (direct / QEMU)
    |
Compare stdout vs .expected
```

## Test Files

Test cases live in `tests/native/` with the pattern:

```
tests/native/
  hello.v              # V source
  hello.expected        # Expected stdout (one line per println)
  fibonacci.v
  fibonacci.expected
  arithmetic.v
  arithmetic.expected
  if_else.v
  if_else.expected
  for_loop.v
  for_loop.expected
  break_continue.v
  break_continue.expected
```

Each `.v` file must include `module main` and a `fn main()` entry point.
The `.expected` file contains the exact stdout the program should produce.

## Prerequisites

### ARM64 Testing

| Platform | Requirements | Notes |
|----------|-------------|-------|
| ARM64 macOS (Apple Silicon) | V compiler, Erlang/OTP | Runs natively |
| x86_64 macOS | Rosetta 2 | `softwareupdate --install-rosetta` |
| x86_64 Linux | `qemu-aarch64` | User-mode emulation, builds ELF not Mach-O |

Install QEMU user-mode (Linux only):

```bash
# Ubuntu/Debian
sudo apt install qemu-user qemu-user-static

# Fedora/RHEL
sudo dnf install qemu-user-static
```

### UEFI Testing

| Requirement | Install Command |
|-------------|----------------|
| `qemu-system-x86_64` | `brew install qemu` (macOS) or `sudo apt install qemu-system-x86` (Ubuntu) |
| OVMF firmware | Bundled with QEMU, or `sudo apt install ovmf` |
| mtools (optional) | `brew install mtools` or `sudo apt install mtools` |

The UEFI test script searches for OVMF firmware in common locations:

- `/usr/share/OVMF/OVMF_CODE.fd`
- `/usr/share/edk2/ovmf/OVMF_CODE.fd`
- `/usr/share/qemu/OVMF.fd`
- `/opt/homebrew/share/qemu/edk2-x86_64-code.fd`
- `tools/OVMF.fd` (project-local)

You can also specify the path explicitly with `--ovmf /path/to/OVMF.fd`.

## Running Tests

### ARM64 Native Tests

```bash
# Run all tests
./scripts/test_native_arm64.sh

# Verbose output (shows compilation steps, diffs on failure)
./scripts/test_native_arm64.sh --verbose

# Run a single test
./scripts/test_native_arm64.sh tests/native/hello.v

# Keep intermediate artifacts for debugging
./scripts/test_native_arm64.sh --keep --verbose
```

### x86_64 UEFI Tests

```bash
# Run all tests
./scripts/test_native_x86_64_uefi.sh

# Verbose output
./scripts/test_native_x86_64_uefi.sh --verbose

# Custom OVMF path
./scripts/test_native_x86_64_uefi.sh --ovmf /path/to/OVMF.fd

# Longer timeout for slow machines
./scripts/test_native_x86_64_uefi.sh --timeout 30

# Keep artifacts (PE binary, EFI structure, disk image)
./scripts/test_native_x86_64_uefi.sh --keep --verbose
```

## Adding New Tests

1. Create `tests/native/my_test.v`:

```v
module main

fn main() {
    println(42)
}
```

2. Create `tests/native/my_test.expected`:

```
42
```

3. Run and verify:

```bash
./scripts/test_native_arm64.sh tests/native/my_test.v --verbose
```

### Guidelines for Test Cases

- Use only integer arithmetic and control flow (the native backend does not yet support
  strings, heap allocation, or runtime calls beyond syscall-based I/O)
- `println()` in native mode outputs integers via syscall write (not Erlang I/O)
- Each `println()` call produces one line in the output
- Keep test programs simple -- the native backend is for bare-metal execution
- Include comments explaining what the test exercises

## Disassembly Guide

When debugging native binaries, disassembly is essential.

### ARM64 Mach-O (macOS)

```bash
# Disassemble the text section
llvm-objdump -d a.out

# With source mapping (if available)
llvm-objdump -d --source a.out

# Show all headers
llvm-objdump --macho --all-headers a.out

# Show load commands
otool -l a.out

# Show the data section
llvm-objdump -s --section=__DATA,__data a.out

# Using the system tools
objdump -d a.out          # Xcode command line tools
otool -tv a.out            # macOS native
```

### x86_64 ELF (Linux)

```bash
# Disassemble
objdump -d a.out

# With Intel syntax
objdump -d -M intel a.out

# Show ELF headers
readelf -a a.out

# Show program headers
readelf -l a.out

# Show section headers
readelf -S a.out
```

### PE/UEFI

```bash
# Using llvm-objdump
llvm-objdump -d BOOTX64.EFI

# Show PE headers
llvm-objdump --all-headers BOOTX64.EFI

# Using objdump (with PE support)
x86_64-w64-mingw32-objdump -d BOOTX64.EFI
```

### Hex Dump

```bash
# Show raw bytes
hexyl a.out | head -40

# Show specific offset range
hexyl a.out --skip 0x1000 --length 256

# Using xxd
xxd a.out | head -40
```

## Troubleshooting

### "V compiler did not generate native IR file"

The V compiler's native IR generation is triggered by the `VBEAM_TARGET` environment variable.
Ensure you are using the correct V compiler build:

```bash
cd /Users/u/tank/ops/tools/dev/vlang && ./v self
VBEAM_TARGET=arm64 v -b beam tests/native/hello.v
ls tests/native/hello.beam/.vbeam_native.ir
```

### "Native compilation failed"

The Erlang-based assembler (`vbeam_native.erl`) needs to be compiled. The test scripts
handle this automatically via `rebar3 compile`, but if you encounter issues:

```bash
cd vbeam_rt && rebar3 compile
```

Or compile manually:

```bash
erlc -o vbeam_rt/ebin vbeam_rt/src/vbeam_native*.erl vbeam_rt/src/vbeam_native_*.erl
```

### "Timeout after 10s"

The binary may be in an infinite loop or stuck on a blocking syscall. Debug with:

```bash
# Keep artifacts
./scripts/test_native_arm64.sh --keep --verbose tests/native/failing_test.v

# Disassemble the binary
llvm-objdump -d beam_output/native_arm64_tests/failing_test/a.out

# Run under lldb (ARM64 Mac)
lldb beam_output/native_arm64_tests/failing_test/a.out
(lldb) run
(lldb) bt    # backtrace on crash
```

### "OVMF firmware not found"

Download OVMF manually:

```bash
# From your distro
sudo apt install ovmf

# Or download from Tianocore
# https://github.com/tianocore/edk2/releases
# Place as: tools/OVMF.fd
```

### "QEMU output mismatch"

UEFI firmware produces boot messages on the serial port. The test script filters common
noise patterns, but custom OVMF builds may produce different output. Use `--verbose` to
see both raw and filtered output:

```bash
./scripts/test_native_x86_64_uefi.sh --verbose tests/native/hello.v
```

### "Segmentation fault" or "Bus error"

Common causes in native binaries:

1. **Stack alignment**: ARM64 requires 16-byte stack alignment at function calls
2. **Data section addressing**: ADRP/ADD pair must be correctly relocated
3. **Syscall numbers**: macOS ARM64 uses x16 for syscall number, Linux uses x8
4. **Page size**: macOS ARM64 uses 16KB pages, Linux uses 4KB

Debug with:

```bash
# ARM64 Mac - run under lldb
lldb a.out
(lldb) run
(lldb) register read    # show register state at crash
(lldb) disassemble       # show code at crash point
(lldb) memory read $x1   # inspect memory

# Linux - run under gdb
gdb ./a.out
(gdb) run
(gdb) info registers
(gdb) disassemble
```

## Architecture Notes

### ARM64 Mach-O (macOS)

- Dynamically linked against `libSystem.B.dylib` (required by macOS)
- Entry via `LC_MAIN` load command (dyld-based)
- Syscalls: number in x16, args in x0-x5, `SVC #0x80`
- 16KB page alignment
- Code signing handled by `codesign` (or ad-hoc)

### x86_64 ELF (Linux)

- Statically linked (no dynamic linker needed)
- Entry via ELF entry point (kernel jumps directly)
- Syscalls: number in rax, args in rdi/rsi/rdx/r10/r8/r9, `syscall`
- 4KB page alignment

### x86_64 PE (UEFI)

- UEFI application subsystem (subsystem type 10)
- Entry via PE AddressOfEntryPoint
- I/O via UEFI boot services (not OS syscalls)
- 512-byte file alignment, 4KB section alignment
- Requires OVMF firmware to boot in QEMU
