# BEAM OS Nucleus

This directory contains the **nucleus** — the minimal bootable core for a BEAM-based OS kernel.

## Quick Start

```bash
# Build nucleus.efi
erl -noshell -pa os/nucleus -pa vbeam_rt/ebin -eval "vbeam_nucleus_boot:build()" -s init stop

# Verify
./os/nucleus/verify_nucleus.sh

# Test in QEMU (requires qemu-system-x86_64 + OVMF firmware)
./scripts/test_nucleus_qemu.sh
```

## What It Does

The nucleus boots via **UEFI** and:

1. **Initializes serial console** (COM1 at 0x3F8 for debug output)
2. **Prints boot banner** ("BEAM Kernel v0.1.0 booting...")
3. **Exits UEFI boot services** (takes control of hardware)
4. **Sets up GDT** (Global Descriptor Table with code/data segments)
5. **Sets up IDT** (Interrupt Descriptor Table with 256 vectors)
6. **Enables paging** (4-level page tables, identity maps first 4GB)
7. **Initializes LAPIC timer** (generates periodic scheduler ticks)
8. **Enters main loop** (HLT + timer interrupts printing "tick")

## Architecture

```
UEFI Firmware
    ↓
nucleus.efi (PE32+ executable)
    ↓
Serial console init
    ↓
Exit UEFI boot services
    ↓
Setup GDT/IDT/paging
    ↓
LAPIC timer (periodic interrupts)
    ↓
Main loop: HLT + "tick" on each interrupt
```

## Files

- **`vbeam_nucleus_boot.erl`** (544 LOC) — Generates nucleus.efi
- **`nucleus.efi`** (30KB) — Bootable UEFI binary (output)
- **`verify_nucleus.sh`** — Validates PE structure
- **`README.md`** — This file

## Memory Layout

```
Physical Memory (Identity Mapped)
0x00000000 - 0x00000FFF    BIOS/firmware reserved
0x00001000 - 0x000FFFFF    Nucleus code/data
0x00100000 - 0x003FFFFF    Free (future: BEAM processes)
0x00400000 - ...           Future: BEAM runtime
...
0xFEE00000 - 0xFEE00FFF    Local APIC MMIO
0xFFFFFFFF                 End of 32-bit space
```

## Next Steps

Future phases will build on this nucleus to create a full BEAM-based OS:

1. **BEAM loader** — Load .beam files from disk
2. **Process scheduler** — Use LAPIC timer ticks for preemption
3. **Memory allocator** — Bitmap allocator for physical pages
4. **Drivers** — VirtIO block/network, keyboard/mouse
5. **BEAM system processes** — kernel, stdlib, custom drivers

## Technical Details

See **`docs/nucleus-boot-sequence.md`** for:
- Complete boot sequence
- GDT/IDT/paging setup
- LAPIC timer configuration
- PE file structure
- Connection to BEAM VM layer

## Requirements

- **Build**: Erlang/OTP 24+ (for `vbeam_native_pe.erl`)
- **Test**: QEMU 7.0+ with UEFI firmware (OVMF)

Install OVMF:
```bash
# macOS
brew install qemu

# Linux
apt install qemu-system-x86 ovmf
```

## Limitations

The nucleus is a **foundation layer** only:
- No disk I/O yet (future: VirtIO block driver)
- No networking yet (future: VirtIO network driver)
- No BEAM runtime yet (future: load .beam files)
- No user processes yet (future: BEAM scheduler)

It establishes the bare-metal environment needed to run BEAM processes directly on hardware.
