# Linux Kernel API (linuxkpi) Surface

Auto-generated compatibility shims for Linux kernel module (.ko) support on BEAM.

## Overview

The linuxkpi layer provides BEAM implementations of common Linux kernel APIs, allowing Linux driver modules to run on the V-on-BEAM OS without modification.

**Implementation Strategy:**
- **Memory management**: Use BEAM heap + ETS tracking
- **Synchronization**: Map to gen_server calls / message passing
- **Interrupts**: Route to BEAM processes
- **Timers/Work**: Use erlang:send_after / spawn
- **Device APIs**: Coordinate with vbeam_* subsystems

## Symbol Categories

### Memory Management (9 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `kmalloc` | ✅ Stub | Allocate BEAM binary, track in ETS |
| `__kmalloc` | ✅ Stub | Alias for kmalloc |
| `kfree` | ✅ Stub | Remove from ETS, GC handles free |
| `vmalloc` | ✅ Stub | Same as kmalloc (no MMU distinction) |
| `__vmalloc` | ✅ Stub | Alias for vmalloc |
| `vfree` | ✅ Stub | Alias for kfree |
| `kzalloc` | ✅ Stub | Zero-initialized allocation (default) |
| `dma_alloc_coherent` | ✅ Stub | Allocate pinned memory, fake DMA addr |
| `dma_free_coherent` | ✅ Stub | Free DMA-coherent memory |

**Next Steps:**
- [ ] Implement ETS-based memory tracking table
- [ ] Add memory leak detection / stats
- [ ] Coordinate with vbeam_dma for real physical addresses

### Logging (6 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `printk` | ✅ Implemented | Route to logger:notice |
| `_printk` | ✅ Implemented | Alias for printk |
| `dev_err` | ✅ Implemented | Route to logger:error |
| `dev_warn` | ✅ Implemented | Route to logger:warning |
| `dev_info` | ✅ Implemented | Route to logger:info |
| `dev_dbg` | ✅ Implemented | Route to logger:debug |

**Status:** Fully functional - logs route to BEAM logger system.

### Synchronization (16 symbols)

#### Spinlocks (7 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `spin_lock_init` | ✅ Stub | Create lock ref in ETS |
| `_raw_spin_lock` | ✅ Stub | gen_server call for serialization |
| `_raw_spin_unlock` | ✅ Stub | gen_server call |
| `spin_lock` | ✅ Stub | Alias for _raw_spin_lock |
| `spin_unlock` | ✅ Stub | Alias for _raw_spin_unlock |
| `spin_lock_irqsave` | ✅ Stub | Same as spin_lock (no IRQ disable) |
| `spin_unlock_irqrestore` | ✅ Stub | Same as spin_unlock |

#### Mutexes (4 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `mutex_init` | ✅ Stub | Create mutex ref in ETS |
| `__mutex_init` | ✅ Stub | Alias for mutex_init |
| `mutex_lock` | ✅ Stub | gen_server call (blocking) |
| `mutex_unlock` | ✅ Stub | gen_server call |

#### Wait Queues (5 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `init_waitqueue_head` | ✅ Stub | Create wait queue registry |
| `__init_waitqueue_head` | ✅ Stub | Alias |
| `wake_up` | ✅ Stub | Send message to all waiters |
| `__wake_up` | ✅ Stub | Alias |
| `wake_up_interruptible` | ✅ Stub | Same as wake_up |

**Next Steps:**
- [ ] Implement lock manager gen_server
- [ ] Add deadlock detection
- [ ] Implement proper wait queue with message passing

### PCI (10 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `pci_register_driver` | ✅ Stub | Register with vbeam_pci |
| `__pci_register_driver` | ✅ Stub | Alias |
| `pci_unregister_driver` | ✅ Stub | Unregister from vbeam_pci |
| `pci_enable_device` | ✅ Stub | Enable MMIO/IO, setup BARs |
| `pci_disable_device` | ✅ Stub | Disable device |
| `pci_set_master` | ✅ Stub | Enable DMA |
| `pci_resource_start` | ✅ Stub | Return BAR base address |
| `pci_resource_len` | ✅ Stub | Return BAR size |
| `pci_iomap` | ✅ Stub | Map BAR to BEAM binary |
| `pci_iounmap` | ✅ Stub | Unmap BAR |

**Next Steps:**
- [ ] Coordinate with vbeam_pci for real BAR addresses
- [ ] Implement MMIO read/write wrappers
- [ ] Add PCI config space access

### Interrupts (5 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `request_irq` | ✅ Stub | Register IRQ handler process |
| `request_threaded_irq` | ✅ Stub | Simplified to request_irq |
| `free_irq` | ✅ Stub | Unregister IRQ handler |
| `enable_irq` | ✅ Stub | Enable IRQ delivery |
| `disable_irq` | ✅ Stub | Disable IRQ delivery |

**Next Steps:**
- [ ] Implement vbeam_irq manager
- [ ] Route hardware IRQs to BEAM messages
- [ ] Add shared IRQ support

### Workqueues & Timers (4 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `schedule_work` | ✅ Stub | spawn BEAM process |
| `queue_work` | ✅ Stub | spawn on workqueue |
| `mod_timer` | ✅ Stub | erlang:send_after |
| `del_timer` | ✅ Stub | erlang:cancel_timer |
| `del_timer_sync` | ✅ Stub | Alias for del_timer |

**Next Steps:**
- [ ] Implement workqueue manager
- [ ] Add timer tracking / cancellation
- [ ] Handle timer callbacks properly

### DMA (4 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `dma_map_single` | ✅ Stub | Pin memory, return phys addr |
| `dma_unmap_single` | ✅ Stub | Unpin memory |
| `dma_set_mask` | ✅ Stub | Set DMA address capability |
| `dma_set_coherent_mask` | ✅ Stub | Alias for dma_set_mask |

**Next Steps:**
- [ ] Coordinate with vbeam_dma
- [ ] Implement memory pinning
- [ ] Add scatter-gather DMA support

### Network Subsystem (12 symbols)

| Symbol | Status | BEAM Implementation |
|--------|--------|---------------------|
| `alloc_etherdev` | ✅ Stub | Create net_device map |
| `alloc_etherdev_mq` | ✅ Stub | Multi-queue variant |
| `register_netdev` | ✅ Stub | Register with vbeam_net |
| `unregister_netdev` | ✅ Stub | Unregister from vbeam_net |
| `netif_start_queue` | ✅ Stub | Enable TX queue |
| `netif_stop_queue` | ✅ Stub | Disable TX queue |
| `netif_wake_queue` | ✅ Stub | Resume TX queue |
| `netif_rx` | ✅ Stub | Receive packet to stack |
| `napi_schedule` | ✅ Stub | Schedule NAPI poll |
| `__napi_schedule` | ✅ Stub | Alias |
| `napi_complete` | ✅ Stub | Complete NAPI poll |
| `napi_complete_done` | ✅ Stub | Alias |

**Next Steps:**
- [ ] Implement vbeam_net subsystem
- [ ] Add sk_buff (packet buffer) structure
- [ ] Wire up to TAP/TUN interface
- [ ] Implement NAPI polling in BEAM

## Implementation Status Summary

| Category | Total Symbols | Stubbed | Implemented | TODO |
|----------|---------------|---------|-------------|------|
| Memory Management | 9 | 9 | 0 | ETS tracking |
| Logging | 6 | 0 | 6 | ✅ Complete |
| Synchronization | 16 | 16 | 0 | Lock manager |
| PCI | 10 | 10 | 0 | vbeam_pci integration |
| Interrupts | 5 | 5 | 0 | IRQ manager |
| Workqueues/Timers | 4 | 4 | 0 | Workqueue manager |
| DMA | 4 | 4 | 0 | vbeam_dma integration |
| Network | 12 | 12 | 0 | vbeam_net subsystem |
| **TOTAL** | **66** | **60** | **6** | - |

## Usage Example

```erlang
%% In .ko loader, resolve symbol:
{ok, {Module, Function, Arity}} = vbeam_kpi_symbols:resolve("kmalloc").
%% Returns: {ok, {vbeam_linuxkpi, kmalloc, 2}}

%% Call from driver code:
{ok, Ptr, Memory} = vbeam_linuxkpi:kmalloc(4096, 0).
%% Allocates 4KB BEAM binary

%% Check if symbol is supported:
true = vbeam_kpi_symbols:is_supported("pci_enable_device").
```

## Adding New Symbols

1. Add implementation to `vbeam_linuxkpi.erl`
2. Add mapping to `vbeam_kpi_symbols.erl` `all_symbols()` map
3. Update this document

## References

- Linux kernel documentation: https://www.kernel.org/doc/html/latest/
- FreeBSD linuxkpi: https://github.com/freebsd/freebsd-src/tree/main/sys/compat/linuxkpi
- Symbol mangling: Linux uses both mangled (`_raw_spin_lock`) and unmangled (`spin_lock`) variants
