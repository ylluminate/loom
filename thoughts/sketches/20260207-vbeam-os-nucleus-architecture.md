# Sketch: V-on-BEAM OS Nucleus — QNX-Like Reliability Architecture

COVERS:
- os/nucleus/vbeam_nucleus_boot.erl
- os/beam_vm/vbeam_beam_interp_v2.erl
- os/compat/linux_syscalls/vbeam_linux_syscall.erl
- os/compat/linux_ko_loader/vbeam_elf_loader.erl
- os/compat/linuxkpi/vbeam_linuxkpi.erl
- vbeam_rt/src/vbeam_native_pe.erl
- vbeam_rt/src/vbeam_native_x86_64.erl

---

## 1. Design Philosophy: BEAM IS the Microkernel

QNX's reliability comes from: tiny trusted kernel + everything else in user-space processes
communicating via message passing. BEAM already IS this model:

| QNX Concept | BEAM Equivalent | Our Implementation |
|---|---|---|
| Microkernel (~100KB) | BEAM scheduler + GC | `vbeam_beam_interp_v2.erl` on bare metal |
| Resource managers | OTP gen_servers | Supervised Erlang processes |
| Message passing IPC | Erlang `!` operator | Zero-copy on bare metal (shared heap region) |
| Process isolation | MMU + address spaces | BEAM process heaps (software isolation) |
| Transparent restart | QNX restart policy | OTP supervisor strategies |
| Interrupt → pulse | QNX pulse mechanism | ISR → BEAM message bridge |
| Adaptive partitioning | QNX APS | Per-process reduction budgets |

**Key insight**: We don't need to BUILD a microkernel — we need to RUN one on bare metal.
The BEAM VM IS the microkernel. Our job is the boot sequence to get there.

---

## 2. System Overview — Full Stack

```mermaid
graph TB
    subgraph "Layer 5: V Applications"
        VA[V Programs<br>compiled to .beam]
        VK[V Kernel Services<br>compiled to .beam]
    end

    subgraph "Layer 4: OTP Supervisor Tree"
        SUP[Root Supervisor<br>one_for_one]
        SUP --> FS[vbeam_fs<br>Filesystem]
        SUP --> NET[vbeam_net<br>Network Stack]
        SUP --> DRV[vbeam_driver_sup<br>Device Drivers]
        SUP --> SCHED[vbeam_scheduler<br>Process Scheduler]
        SUP --> CONS[vbeam_console<br>Serial Console]
        SUP --> COMPAT[vbeam_linux_compat<br>Linux Compatibility]
    end

    subgraph "Layer 3: BEAM Virtual Machine"
        INTERP[vbeam_beam_interp_v2<br>Bytecode Interpreter]
        PROC[Process Scheduler<br>Preemptive, reduction-counted]
        GC[Per-Process GC<br>Generational, no global pause]
        MSG[Message Passing<br>Mailbox per process]
        ETS[ETS Tables<br>Shared key-value store]
    end

    subgraph "Layer 2: Hardware Abstraction Layer"
        SERIAL[Serial Port<br>COM1 @ 0x3F8]
        TIMER[LAPIC Timer<br>ISR → message bridge]
        PCI[PCI Enumeration<br>Config space access]
        MMIO[MMIO Regions<br>Device registers]
        DMA_HW[DMA Engine<br>Buffer management]
    end

    subgraph "Layer 1: Boot / Bare Metal"
        UEFI[UEFI Firmware<br>PE32+ entry point]
        GDT[GDT/IDT Setup<br>Segments + Interrupts]
        PAGING[4-Level Paging<br>Identity map 4GB]
        X86[x86_64 Privileged Ops<br>vbeam_native_x86_64.erl]
    end

    VA --> INTERP
    VK --> INTERP
    FS & NET & DRV & SCHED & CONS & COMPAT --> INTERP
    INTERP --> PROC & GC & MSG & ETS
    PROC --> TIMER
    MSG --> SERIAL
    DRV --> PCI & MMIO & DMA_HW
    TIMER --> GDT
    SERIAL --> X86
    GDT --> PAGING
    PAGING --> UEFI
```

---

## 3. Boot Sequence — UEFI to V Hello World

```mermaid
sequenceDiagram
    participant FW as UEFI Firmware
    participant NUC as Nucleus Boot<br>(x86_64 asm)
    participant BEAM as BEAM Interpreter<br>(Erlang on bare metal)
    participant SUP as OTP Supervisors
    participant APP as V Application

    FW->>NUC: Load BOOTX64.EFI from FAT32
    Note over NUC: Phase 1: Hardware Init
    NUC->>NUC: Init COM1 serial (115200 8N1)
    NUC->>NUC: Print "BEAM Kernel v0.1.0 booting..."
    NUC->>NUC: ExitBootServices()

    Note over NUC: Phase 2: CPU Setup
    NUC->>NUC: Load GDT (code64 + data64)
    NUC->>NUC: Load IDT (256 entries, ISR stubs)
    NUC->>NUC: Setup 4-level paging (identity map 4GB)
    NUC->>NUC: Enable LAPIC timer (periodic, vector 32)

    Note over NUC: Phase 3: Memory Manager
    NUC->>NUC: Initialize physical page allocator
    NUC->>NUC: Setup kernel heap (slab allocator)

    Note over BEAM: Phase 4: BEAM Bootstrap
    NUC->>BEAM: Jump to interpreter entry
    BEAM->>BEAM: Initialize process table
    BEAM->>BEAM: Initialize message queues
    BEAM->>BEAM: Load embedded .beam modules

    Note over SUP: Phase 5: OTP Boot
    BEAM->>SUP: Start root supervisor
    SUP->>SUP: Start vbeam_console (serial I/O)
    SUP->>SUP: Start vbeam_scheduler
    SUP->>SUP: Start vbeam_driver_sup
    SUP->>SUP: Start vbeam_fs (RAM disk)

    Note over APP: Phase 6: Application
    SUP->>APP: Start V application
    APP->>APP: v.main:main()
    APP->>BEAM: io:format("Hello from V!")
    BEAM->>NUC: Serial write via port driver
```

---

## 4. QNX Reliability Patterns — Mapped to BEAM

### 4a. Supervisor Tree (Heart of Reliability)

```mermaid
graph TB
    subgraph "Restart Strategy: rest_for_one"
        ROOT[vbeam_kernel_sup<br>rest_for_one]
    end

    ROOT --> HAL[vbeam_hal_sup<br>one_for_one]
    ROOT --> SVC[vbeam_service_sup<br>one_for_one]
    ROOT --> APP_SUP[vbeam_app_sup<br>simple_one_for_one]

    subgraph "Hardware Abstraction (crash = restart driver)"
        HAL --> SER[vbeam_serial<br>gen_server<br>COM1 port owner]
        HAL --> TIMER_SVC[vbeam_timer<br>gen_server<br>LAPIC config]
        HAL --> PCI_SVC[vbeam_pci<br>gen_server<br>PCI enumeration]
        HAL --> BLK[vbeam_block<br>gen_server<br>IDE/AHCI driver]
    end

    subgraph "OS Services (crash = restart service, keep drivers)"
        SVC --> VFS[vbeam_vfs<br>gen_server<br>Virtual FS layer]
        SVC --> NET_SVC[vbeam_net<br>gen_server<br>TCP/IP stack]
        SVC --> CONSOLE[vbeam_console<br>gen_server<br>Serial shell]
        SVC --> SYSCALL[vbeam_linux_compat<br>gen_server<br>Syscall dispatch]
    end

    subgraph "User Applications (crash = restart app only)"
        APP_SUP --> APP1[V App 1<br>gen_server]
        APP_SUP --> APP2[V App 2<br>gen_server]
        APP_SUP --> APP3[V App N<br>gen_server]
    end

    style ROOT fill:#ff6666,color:#fff
    style HAL fill:#ff9944,color:#fff
    style SVC fill:#4488ff,color:#fff
    style APP_SUP fill:#44bb44,color:#fff
```

**Why `rest_for_one` at root**: If HAL crashes, services that depend on it also restart.
If a service crashes, HAL keeps running (drivers stay alive). Apps are independent.

### 4b. Crash Isolation Guarantees

```
Driver crashes?
  → HAL supervisor restarts just that driver
  → Other drivers unaffected
  → Services temporarily lose access, retry on reconnect

Service crashes?
  → Service supervisor restarts it
  → Drivers keep running (hardware state preserved)
  → Applications get {:EXIT, ...} messages, can handle gracefully

Application crashes?
  → App supervisor restarts it
  → Zero impact on drivers or services
  → Other applications unaffected

Interpreter bug?
  → This is the one thing that kills everything
  → Mitigation: watchdog timer (hardware), triple modular redundancy for critical paths
```

---

## 5. Interrupt → Message Bridge

```mermaid
sequenceDiagram
    participant HW as Hardware<br>(LAPIC/PCI/Serial)
    participant ISR as ISR Stub<br>(x86_64 asm, ~20 instructions)
    participant RING as Interrupt Ring Buffer<br>(shared memory)
    participant POLL as BEAM Poller Process<br>(high priority)
    participant TGT as Target Process<br>(gen_server)

    HW->>ISR: Interrupt fires (vector N)
    Note over ISR: Save registers<br>Disable further IRQs
    ISR->>RING: Write {vector, timestamp, data} to ring
    ISR->>ISR: Send EOI to LAPIC
    ISR->>ISR: Restore registers, IRETQ

    Note over POLL: Runs every reduction slice
    POLL->>RING: Check ring buffer head != tail
    RING->>POLL: Read pending interrupts
    POLL->>TGT: Send {interrupt, Vector, Data} message
    TGT->>TGT: Handle interrupt in gen_server
```

**Why a ring buffer, not direct message send from ISR?**
- ISR runs with interrupts disabled — must be FAST (< 1 microsecond)
- BEAM message send involves heap allocation — too slow/complex for ISR
- Ring buffer is lock-free, fixed-size, O(1) write
- BEAM poller process converts hardware events to messages safely

**This is exactly how QNX does it**: hardware interrupts create "pulses" (lightweight messages)
that are delivered to resource manager threads. Our ring buffer IS the pulse mechanism.

---

## 6. Memory Architecture

```mermaid
graph TB
    subgraph "Physical Memory Map"
        direction TB
        M0["0x00000000 - 0x000FFFFF<br>Legacy (1MB)<br>BIOS, VGA, ROM"]
        M1["0x00100000 - 0x003FFFFF<br>Kernel Code (3MB)<br>Nucleus + BEAM interpreter"]
        M2["0x00400000 - 0x00FFFFFF<br>Kernel Heap (12MB)<br>BEAM process heaps, ETS"]
        M3["0x01000000 - 0x0FFFFFFF<br>Page Pool (240MB)<br>Physical page allocator"]
        M4["0xFEE00000<br>LAPIC MMIO"]
        M5["0xFED00000<br>HPET MMIO"]
    end

    subgraph "BEAM Process Memory Model"
        direction TB
        P1[Process 1 Heap<br>64KB initial, grows]
        P2[Process 2 Heap<br>64KB initial, grows]
        P3[Process 3 Heap<br>64KB initial, grows]
        SHARED[Shared Heap Region<br>Large binaries,<br>refc counted]
        MBOX1[Mailbox 1]
        MBOX2[Mailbox 2]
        MBOX3[Mailbox 3]
    end

    M2 --> P1 & P2 & P3 & SHARED
    P1 --> MBOX1
    P2 --> MBOX2
    P3 --> MBOX3
```

**QNX comparison**:
- QNX uses MMU for process isolation (hardware enforced)
- BEAM uses software isolation (each process has its own heap, no pointer sharing)
- For Phase 1: software isolation is sufficient (BEAM already guarantees it)
- For Phase 2: we CAN add MMU-backed isolation for defense-in-depth
  (each BEAM process in its own page table, like Singularity OS)

---

## 7. Driver Model — gen_server Based

```mermaid
classDiagram
    class vbeam_driver_behaviour {
        <<behaviour>>
        +init(Config) State
        +handle_interrupt(Vector, Data, State) State
        +handle_call(Request, From, State) Reply
        +handle_cast(Request, State) State
        +terminate(Reason, State) void
    }

    class vbeam_serial_driver {
        -port: 0x3F8
        -baud: 115200
        +init(Config)
        +handle_interrupt(4, Data, State)
        +write(Bytes)
        +read()
    }

    class vbeam_block_driver {
        -controller: AHCI
        -dma_buf: PhysAddr
        +init(Config)
        +handle_interrupt(Vector, Data, State)
        +read_sectors(LBA, Count)
        +write_sectors(LBA, Count, Data)
    }

    class vbeam_net_driver {
        -device: e1000/virtio
        -rx_ring: DMABuffer
        -tx_ring: DMABuffer
        +init(Config)
        +handle_interrupt(Vector, Data, State)
        +send_packet(Frame)
        +recv_packet()
    }

    vbeam_driver_behaviour <|-- vbeam_serial_driver
    vbeam_driver_behaviour <|-- vbeam_block_driver
    vbeam_driver_behaviour <|-- vbeam_net_driver
```

**Each driver is a supervised gen_server**:
- Owns its hardware resources (I/O ports, MMIO regions, IRQ vectors)
- Registers for interrupts via the interrupt bridge
- Crashes are caught by `vbeam_hal_sup` and the driver restarts
- Hardware state is re-initialized on restart (like QNX resource manager restart)

---

## 8. Linux Compatibility Layer

```mermaid
flowchart TD
    subgraph "V Application (compiled to .beam)"
        VAPP[V code calling<br>C.open, C.read, C.write]
    end

    subgraph "Syscall Bridge"
        STUB[vbeam codegen intercepts<br>C function calls]
        STUB --> DISPATCH[vbeam_linux_syscall:dispatch/2<br>~450 syscalls]
    end

    subgraph "LinuxKPI Shims"
        DISPATCH --> |"open/read/write"| VFS_SHIM[vbeam_vfs gen_server]
        DISPATCH --> |"socket/connect/send"| NET_SHIM[vbeam_net gen_server]
        DISPATCH --> |"mmap/brk"| MEM_SHIM[Memory manager]
        DISPATCH --> |"ioctl"| DRV_SHIM[Driver dispatch]
    end

    subgraph "Kernel Module Loading"
        KO[Linux .ko files<br>ELF64 relocatable]
        KO --> ELF[vbeam_elf_loader:parse/1<br>Parse ELF headers + sections]
        ELF --> RESOLVE[resolve_symbols/2<br>Link against vbeam_linuxkpi]
        RESOLVE --> LOAD[apply_relocations/2<br>Fixup addresses]
        LOAD --> EXEC[Execute init_module()]
    end

    VAPP --> STUB
    VFS_SHIM --> |"file ops"| BLK_DRV[vbeam_block_driver]
    NET_SHIM --> |"packets"| NET_DRV[vbeam_net_driver]
```

**This is our "personality layer"** (QNX term): the BEAM kernel speaks BEAM natively,
but can present a Linux personality for compatibility with existing V programs that call
POSIX functions. Just like QNX can run POSIX apps on its microkernel.

---

## 9. What Exists Now vs What's Needed

### Exists (Built in Wave 2)

| Component | File | Status | LOC |
|---|---|---|---|
| UEFI boot | `os/nucleus/vbeam_nucleus_boot.erl` | Working in QEMU | 577 |
| PE32+ emitter | `vbeam_rt/src/vbeam_native_pe.erl` | Working, 2 bugs fixed | 361 |
| BEAM interpreter | `os/beam_vm/vbeam_beam_interp_v2.erl` | Compiles, untested on bare metal | ~400 |
| ELF loader | `os/compat/linux_ko_loader/vbeam_elf_loader.erl` | Compiles | ~500 |
| Linux syscalls | `os/compat/linux_syscalls/vbeam_linux_syscall.erl` | Compiles, ~450 stubs | ~800 |
| LinuxKPI | `os/compat/linuxkpi/vbeam_linuxkpi.erl` | Compiles, 66 shims | ~600 |
| x86_64 encoder | `vbeam_rt/src/vbeam_native_x86_64.erl` | Working, 53 privileged ops | ~1700 |

### Needed (Wave 3 — Getting to Bare-Metal V)

| Phase | Component | Description | Depends On |
|---|---|---|---|
| 3a | Full boot sequence | GDT + IDT + paging in nucleus | Nucleus (done) |
| 3b | Memory manager | Physical page allocator + kernel heap | Full boot |
| 3c | Interrupt bridge | ISR stubs → ring buffer → BEAM poller | IDT + memory |
| 3d | BEAM on bare metal | Wire interp_v2 into nucleus | Memory + interrupts |
| 3e | Serial driver | gen_server wrapping COM1 | BEAM running |
| 3f | Console shell | Interactive Erlang shell over serial | Serial driver |
| 3g | V hello world | Compile V → .beam → run on bare metal | Console |

### Future (Wave 4 — Production Reliability)

| Phase | Component | Description |
|---|---|---|
| 4a | PCI enumeration | Discover devices on PCI bus |
| 4b | Block driver | IDE/AHCI disk driver (gen_server) |
| 4c | Network driver | e1000/virtio-net driver (gen_server) |
| 4d | VFS layer | Virtual filesystem with RAM disk |
| 4e | TCP/IP stack | Lightweight TCP/IP in Erlang |
| 4f | MMU isolation | Hardware-backed process isolation |
| 4g | Watchdog | Hardware timer-based health monitor |
| 4h | Hot code reload | OTP release handler on bare metal |

---

## 10. Wave 3 Implementation Order (Critical Path)

```mermaid
gantt
    title Wave 3: UEFI → Bare-Metal V Hello World
    dateFormat X
    axisFormat %s

    section Boot
    Full boot (GDT+IDT+Paging)    :a1, 0, 2
    Memory manager (pages+heap)    :a2, after a1, 2

    section Interrupts
    ISR stubs + ring buffer        :b1, after a2, 1
    BEAM poller process            :b2, after b1, 1

    section BEAM Runtime
    Wire interp_v2 into nucleus    :c1, after a2, 2
    Embed .beam modules in PE      :c2, after c1, 1
    Process scheduler              :c3, after c2 b2, 2

    section I/O
    Serial port driver             :d1, after c3, 1
    Console shell                  :d2, after d1, 1

    section Milestone
    V hello world on bare metal    :milestone, after d2, 0
```

---

## 11. What Must NOT Break (Invariants)

1. **ISR latency < 1 microsecond** — ISR stubs do ring buffer write + EOI, nothing else
2. **No heap allocation in ISR context** — all allocation happens in BEAM processes
3. **Supervisor tree never has single points of failure** — every critical service is supervised
4. **Driver crash never corrupts kernel state** — drivers own their state, supervisor restarts clean
5. **BEAM GC never pauses other processes** — per-process heaps, generational GC
6. **Message passing is the ONLY IPC** — no shared mutable state between processes
7. **Serial console always recoverable** — if console process crashes, supervisor restarts it

## 12. How We'll Verify

- [ ] `make qemu` — Full boot to serial output (exists, passing)
- [ ] `make qemu-interp` — BEAM interpreter executes hello.beam on bare metal
- [ ] `make qemu-otp` — OTP supervisor tree starts, serial console interactive
- [ ] `make qemu-vhello` — V-compiled .beam runs on bare metal, prints to serial
- [ ] Stress test: spawn 10,000 processes, verify supervisor restarts on crashes
- [ ] Chaos test: kill random driver processes, verify automatic restart
- [ ] Latency test: measure interrupt-to-message time, must be < 100 microseconds
