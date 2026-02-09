# BEAM OS Nucleus Boot Sequence

## Overview

The **nucleus** is the minimal bootable core of the BEAM-based OS kernel. It boots via UEFI, takes control of the hardware, and establishes the bare-metal environment needed to run BEAM processes.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ UEFI Firmware                                                │
│ - Initializes hardware                                       │
│ - Loads BOOTX64.EFI (our nucleus)                           │
│ - Calls efi_main(ImageHandle, SystemTable)                  │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ Nucleus Boot (nucleus.efi)                                   │
│                                                               │
│ Phase 1: UEFI Environment                                    │
│   1. Initialize serial console (COM1 at 0x3F8)               │
│   2. Print boot banner                                       │
│   3. Exit UEFI boot services                                 │
│                                                               │
│ Phase 2: Hardware Setup                                      │
│   4. Load GDT (Global Descriptor Table)                      │
│   5. Load IDT (Interrupt Descriptor Table)                   │
│   6. Setup 4-level page tables (identity map 4GB)            │
│   7. Enable paging and long mode                             │
│   8. Initialize LAPIC timer                                  │
│                                                               │
│ Phase 3: Main Loop                                           │
│   9. Enter scheduler loop (HLT + interrupts)                 │
│  10. Timer ISR prints "tick" on each interrupt               │
└─────────────────────────────────────────────────────────────┘
```

## Memory Layout

### Physical Memory (First 4GB)

```
0x00000000 - 0x00000FFF   BIOS/firmware reserved
0x00001000 - 0x000FFFFF   Nucleus code/data (loaded by UEFI)
0x00100000 - 0x003FFFFF   Free (kernel heap, BEAM processes)
0x00400000 - ...          Future: BEAM runtime
...
0xFEE00000 - 0xFEE00FFF   Local APIC MMIO
0xFFFFFFFF                End of 32-bit space
```

### Virtual Memory (Identity Mapped)

The nucleus uses **4-level paging** (x86_64 long mode):

```
PML4 (Page Map Level 4)
  └─ PML4[0] -> PDPT (Page Directory Pointer Table)
       ├─ PDPT[0] -> PD0 (covers 0x00000000 - 0x3FFFFFFF)
       ├─ PDPT[1] -> PD1 (covers 0x40000000 - 0x7FFFFFFF)
       ├─ PDPT[2] -> PD2 (covers 0x80000000 - 0xBFFFFFFF)
       └─ PDPT[3] -> PD3 (covers 0xC0000000 - 0xFFFFFFFF)

Each PD uses 2MB pages (PS=1):
  PD[0] = 0x00000000 (2MB, identity mapped)
  PD[1] = 0x00200000 (2MB, identity mapped)
  ...
  PD[511] = 0x3FE00000 (2MB, identity mapped)
```

**Total mapped**: 4GB (identity map: virtual address = physical address)

## GDT (Global Descriptor Table)

```
Entry 0: Null descriptor (required by x86_64)
Entry 1: Code segment (64-bit, ring 0, executable, readable)
Entry 2: Data segment (64-bit, ring 0, writable)
```

Segment selectors:
- CS = 0x0008 (code segment)
- DS/ES/SS/FS/GS = 0x0010 (data segment)

## IDT (Interrupt Descriptor Table)

256 interrupt vectors:

```
Vector 0-31:   CPU exceptions (divide error, page fault, etc.)
Vector 32:     Timer interrupt (LAPIC timer) -> timer_isr
Vector 33-255: Available for future use
```

**Timer ISR** (Interrupt Service Routine):
1. Save all registers (push rax, rcx, rdx, ...)
2. Print "tick\r\n" to serial console
3. Send EOI (End Of Interrupt) to LAPIC
4. Restore all registers (pop rdi, rsi, ...)
5. Return from interrupt (iretq)

## LAPIC Timer Setup

The **Local APIC** (Advanced Programmable Interrupt Controller) timer generates periodic interrupts for the scheduler.

Configuration:
- **Base address**: 0xFEE00000 (MMIO)
- **Divide value**: 16 (DCR = 3)
- **Initial count**: 0x100000 (determines tick rate)
- **Mode**: Periodic
- **Vector**: 32 (IRQ 0 remapped to interrupt 32)

Approximate tick rate:
```
CPU frequency / (divide * initial_count) ≈ 100 Hz (10ms ticks)
```

## Serial Console

The nucleus uses **COM1** (UART 16550) for debug output:

- **Base port**: 0x3F8
- **Baud rate**: 115200
- **Format**: 8 data bits, no parity, 1 stop bit (8N1)
- **FIFO**: Enabled

Registers:
- 0x3F8: Transmit/Receive buffer
- 0x3F9: Interrupt Enable Register
- 0x3FA: FIFO Control Register
- 0x3FB: Line Control Register
- 0x3FC: Modem Control Register
- 0x3FD: Line Status Register

Output routine:
1. Wait for THRE (Transmitter Holding Register Empty) in LSR
2. Write byte to THR (0x3F8)

## Boot Sequence (Detailed)

### 1. UEFI Entry

```c
EFI_STATUS efi_main(EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable)
```

UEFI firmware calls `efi_main` with:
- `ImageHandle`: Opaque handle to our .efi image
- `SystemTable`: Pointer to UEFI system table (boot/runtime services)

### 2. Serial Initialization

Initialize COM1 UART:
- Disable interrupts (IER = 0)
- Set baud rate divisor (115200 baud: divisor = 1)
- Configure line format (8N1)
- Enable FIFO (FCR = 0xC7)
- Enable DTR and RTS (MCR = 0x0B)

### 3. Print Boot Banner

Output to serial:
```
BEAM Kernel v0.1.0 booting...
```

### 4. Exit UEFI Boot Services

After exiting boot services, the firmware gives us full control:
- No more UEFI runtime services available
- All hardware is ours to manage
- We must handle all interrupts, memory, I/O

### 5. Load GDT

Execute `lgdt [gdt_ptr]` to load the Global Descriptor Table.

Perform a far jump to reload CS (code segment):
```asm
jmp far 0x0008:reload_cs
```

Reload all segment registers with the data segment (0x0010).

### 6. Load IDT

Execute `lidt [idt_ptr]` to load the Interrupt Descriptor Table.

All 256 vectors are initialized:
- Vector 32 points to `timer_isr`
- Others point to a generic trap handler (stub)

### 7. Enable Paging

**Enable PAE** (Physical Address Extension):
```asm
mov rax, cr4
or rax, 0x20        ; CR4.PAE = 1
mov cr4, rax
```

**Enable long mode** (IA32_EFER.LME):
```asm
mov ecx, 0xC0000080 ; EFER MSR
rdmsr
bts eax, 8          ; EFER.LME = 1
wrmsr
```

**Load page tables**:
```asm
mov cr3, [pml4_address]
```

**Enable paging** (CR0.PG):
```asm
mov rax, cr0
or rax, 0x80000000  ; CR0.PG = 1
mov cr0, rax
```

Now we're in **64-bit long mode** with paging enabled.

### 8. Setup LAPIC Timer

**Enable LAPIC** (write to SVR at offset 0xF0):
```
[LAPIC + 0xF0] = 0x1FF  ; Spurious vector = 0xFF, APIC enabled
```

**Set timer divide** (DCR at offset 0x3E0):
```
[LAPIC + 0x3E0] = 3     ; Divide by 16
```

**Set initial count** (ICR at offset 0x380):
```
[LAPIC + 0x380] = 0x100000  ; Determines tick rate
```

**Set LVT Timer** (at offset 0x320):
```
[LAPIC + 0x320] = 0x20020   ; Periodic mode, vector 32
```

**Enable interrupts**:
```asm
sti
```

### 9. Main Loop

Enter an infinite loop:
```asm
main_loop:
    hlt         ; Halt until next interrupt
    jmp main_loop
```

The CPU will halt and wake on each timer interrupt.

### 10. Timer ISR

On each timer tick (every ~10ms):
1. **Save context**: Push all general-purpose registers
2. **Print "tick\r\n"** to serial console
3. **Send EOI** to LAPIC: `[LAPIC + 0xB0] = 0`
4. **Restore context**: Pop all registers
5. **Return**: `iretq` (return from interrupt)

## PE File Structure

The nucleus is emitted as a **PE32+ (64-bit) UEFI executable**:

```
┌─────────────────────────────┐
│ DOS Header (64 bytes)        │
│ - e_magic = "MZ"             │
│ - e_lfanew -> PE signature   │
├─────────────────────────────┤
│ PE Signature (4 bytes)       │
│ - "PE\0\0"                   │
├─────────────────────────────┤
│ COFF Header (20 bytes)       │
│ - Machine = 0x8664 (AMD64)   │
│ - NumberOfSections = 3       │
├─────────────────────────────┤
│ Optional Header (240 bytes)  │
│ - Subsystem = 10 (EFI app)   │
│ - EntryPoint = .text + 0     │
├─────────────────────────────┤
│ Section Table (120 bytes)    │
│ - .text (code)               │
│ - .data (GDT, IDT, tables)   │
│ - .reloc (relocations)       │
├─────────────────────────────┤
│ .text Section                │
│ - Machine code               │
├─────────────────────────────┤
│ .data Section                │
│ - GDT (24 bytes)             │
│ - IDT (256 * 16 = 4096 bytes)│
│ - Page tables (6 * 4KB)      │
│ - String data                │
├─────────────────────────────┤
│ .reloc Section               │
│ - Base relocation table      │
└─────────────────────────────┘
```

## Testing

Use the provided QEMU test script:

```bash
./scripts/test_nucleus_qemu.sh
```

Expected output:
```
=== BEAM Kernel Nucleus Test ===

[1/4] Building nucleus.efi...
✓ Built: .../nucleus.efi (8192 bytes)

[2/4] Creating EFI boot filesystem...
✓ Created: /tmp/beam_os_boot/EFI/BOOT/BOOTX64.EFI

[3/4] Locating UEFI firmware...
✓ Found UEFI firmware: /opt/homebrew/share/qemu/edk2-x86_64-code.fd

[4/4] Launching QEMU...
----------------------------------------
Press Ctrl+A, X to exit QEMU
Serial output will appear below:
----------------------------------------

BEAM Kernel v0.1.0 booting...
tick
tick
tick
...
```

## Connection to BEAM VM

The nucleus is the **foundation layer** for the BEAM-based OS. Future phases will:

1. **Phase 2**: Load BEAM runtime from disk
   - Parse .beam files
   - Setup BEAM process table
   - Initialize BEAM memory allocator

2. **Phase 3**: Start scheduler
   - Use LAPIC timer ticks for preemption
   - Schedule BEAM processes (round-robin, then priority)
   - Handle BEAM traps and BIFs

3. **Phase 4**: Add drivers
   - VirtIO block device (disk I/O)
   - VirtIO network (networking)
   - Keyboard/mouse input

4. **Phase 5**: BEAM system processes
   - `kernel` application
   - `stdlib` application
   - Custom BEAM-based drivers

The nucleus provides:
- **Hardware abstraction**: GDT, IDT, paging, interrupts
- **Serial console**: For BEAM I/O (stdout/stderr)
- **Timer ticks**: For BEAM scheduler preemption
- **Identity-mapped memory**: BEAM can allocate from 1MB+ region

## Technical Notes

### Why Identity Mapping?

Identity mapping (virtual address = physical address) simplifies early boot:
- No need to translate addresses in assembly code
- UEFI firmware loaded us into physical memory, we just map it 1:1
- Later, BEAM processes can use higher-half kernel (virtual 0xFFFFFFFF80000000+)

### Why 2MB Pages?

Using 2MB pages (instead of 4KB) in the page directory:
- **Fewer page tables**: 4GB requires only 2048 entries (vs 1M entries for 4KB pages)
- **Faster TLB**: Fewer TLB entries needed
- **Simpler setup**: We only need PML4, PDPT, and 4 PDs (no PT level)

Trade-off: Less granular protection (entire 2MB regions have same permissions).

### Why LAPIC Timer?

The Local APIC timer is preferred over PIT (8254 timer) or HPET:
- **Per-core**: Each CPU core has its own LAPIC (future SMP support)
- **Modern**: LAPIC is standard on all x86_64 CPUs
- **Flexible**: Programmable divider and initial count

### Serial Console vs Framebuffer

We use serial console instead of VGA/framebuffer because:
- **Simpler**: No need to parse UEFI GOP (Graphics Output Protocol)
- **Debuggable**: Output appears in QEMU stdio (redirectable to file)
- **Portable**: Works on headless servers and VMs
- **Fast**: Character output is fast compared to pixel manipulation

Future: Add framebuffer support for graphical UI.

## File Locations

```
os/nucleus/
├── vbeam_nucleus_boot.erl   # Nucleus generator (this file)
└── nucleus.efi               # Generated UEFI binary (output)

scripts/
└── test_nucleus_qemu.sh      # QEMU test harness

docs/
└── nucleus-boot-sequence.md  # This document
```

## Next Steps

1. **Add exception handlers**: Proper page fault, GPF, etc. handlers
2. **Memory allocator**: Bitmap allocator for physical pages
3. **BEAM loader**: Parse .beam files, load into memory
4. **BEAM scheduler**: Round-robin scheduling of BEAM processes
5. **BIF stubs**: Implement native BIFs (send, receive, spawn)
6. **System calls**: Interface between BEAM and kernel

## References

- [UEFI Specification 2.10](https://uefi.org/specifications)
- [Intel 64 and IA-32 Architectures Software Developer's Manual](https://www.intel.com/sdm)
- [OSDev Wiki: UEFI](https://wiki.osdev.org/UEFI)
- [OSDev Wiki: Setting Up Long Mode](https://wiki.osdev.org/Setting_Up_Long_Mode)
- [BEAM Book: Internal Data Representation](https://blog.stenmans.org/theBeamBook/)
