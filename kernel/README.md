# kernel/ — Loom OS Kernel

Core kernel subsystems for bare-metal BEAM execution.

## Subsystems

| Directory | Module(s) | Purpose |
|-----------|-----------|---------|
| `boot/` | `vbeam_boot_sequence` | Orchestrates GDT→IDT→paging→stack→kernel startup |
| `mm/` | `vbeam_page_alloc`, `vbeam_paging`, `vbeam_heap` | Physical page allocator (4KB bitmap), x86_64 4-level page tables (2MB identity-mapped), BEAM process heap |
| `sched/` | `vbeam_scheduler` | Round-robin preemptive scheduler triggered by timer IRQ |
| `io/` | `vbeam_io_server`, `vbeam_irq_bridge` | Serial/framebuffer output, hardware IRQ → BEAM message bridge |
| `arch/` | `vbeam_gdt_idt` | x86_64 GDT and IDT setup for bare-metal execution |

## Tests

All kernel tests live in `tests/kernel/` (one `*_test.erl` per module). Run with:

```bash
make test-kernel
```
