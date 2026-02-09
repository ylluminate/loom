# compat/ — Linux Compatibility Layer

Enables running Linux kernel modules and applications on Loom OS.

## Subsystems

| Directory | Module(s) | Purpose |
|-----------|-----------|---------|
| `syscall/` | `vbeam_linux_syscall` | x86_64 Linux syscall dispatch table (~450 syscalls mapped, 50 critical implemented) |
| `elf/` | `vbeam_elf_loader` | ELF64 relocatable object loader — parses .ko kernel modules, resolves x86_64 relocations |
| `kpi/` | `vbeam_linuxkpi`, `vbeam_kapi_symbols` | Linux Kernel Programming Interface shims (66 functions: kmalloc, printk, mutex_lock, etc.) + kernel symbol resolver for .ko imports |

## Strategy

Rather than reimplementing Linux, Loom provides a thin compatibility layer:

1. **Syscalls**: Map Linux syscall numbers to BEAM message passing
2. **ELF Loading**: Parse .ko modules and resolve their symbols against our KPI shims
3. **KPI Shims**: Implement the ~66 most commonly used Linux kernel API functions

This allows reusing existing Linux drivers as supervised BEAM processes.
