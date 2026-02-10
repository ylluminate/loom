# Codex Review — Round 25

**Date**: 2026-02-10
**Findings**: 0 CRITICAL, 5 HIGH, 3 MEDIUM, 1 LOW = 9 total (0 repeats filtered)

## HIGH
1. **x86_64:42/45** — compute_frame_size parity logic leaves rsp % 16 == 8 before nested calls; violates SysV AMD64 16-byte alignment.
2. **jit:104/111** — serial_puts_code stale hardcoded displacements after R24 CLD insertion; call offset off by 1, jz lands mid-instruction.
3. **native:534/536** — ARM64 sqrt builtin FMOV encodings wrong (0x9E27 should be 0x9E67 for fmov d0,x0).
4. **elf_loader:800/764** — find_init_addr picks undefined symbols (shndx=0); init_addr can point outside module text.
5. **interp_bare:347/818/893** — deallocate_stack/nthtail_bare no bounds check; invalid N silently wipes Y stack.

## MEDIUM
6. **arm64:121/1602** — Spill-slot loads/stores use immediate-offset with no large-offset fallback; exceeds imm12 range → codegen crash.
7. **syscall:233/264** — ARCH_GET_FS/GS pointer validation doesn't require Addr >= 0; negative page-aligned values accepted.
8. **irq_bridge:308** — tick delivers IRQs with raw Pid ! Msg, unbounded mailbox growth under flood.

## LOW
9. **boot_sequence:130** — boot_data hard assertions (true = ...) crash instead of returning structured error.
