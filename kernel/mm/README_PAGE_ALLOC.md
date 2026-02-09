# Page Frame Allocator (vbeam_page_alloc)

Physical memory allocator for Loom OS bare-metal kernel.

## Features

- **Bitmap-based tracking**: 1 bit per 4KB page (up to 4GB = 128KB bitmap)
- **First-fit allocation** with hint optimization
- **Contiguous allocation** for DMA buffers
- **Reserved region marking** for kernel/UEFI structures
- **Pure Erlang** - no OTP dependencies (bare-metal compatible)

## API Overview

```erlang
%% Initialize with total physical memory
State = vbeam_page_alloc:init(16 * 1024 * 1024).  % 16MB

%% Allocate single page
{ok, PhysAddr, State1} = vbeam_page_alloc:alloc_page(State).

%% Allocate contiguous block (for DMA)
{ok, StartAddr, State2} = vbeam_page_alloc:alloc_contiguous(State1, 16).

%% Mark UEFI/framebuffer as reserved
State3 = vbeam_page_alloc:mark_reserved(State2, 16#E00000, 16#1000000).

%% Free pages
State4 = vbeam_page_alloc:free_page(State3, PhysAddr).

%% Statistics
Stats = vbeam_page_alloc:stats(State4).
%% => #{total => 4096, free => 3580, allocated => 516}
```

## Design Notes

- **Page size**: 4096 bytes (4KB)
- **Auto-reserved**: First 2MB for kernel + boot structures
- **Bitmap**: Binary where bit 0 = free, bit 1 = allocated
- **Optimization**: `next_hint` tracks last allocation for fast scanning

## Testing

```bash
erlc -o os/kernel os/kernel/vbeam_page_alloc.erl
erlc -o os/kernel os/kernel/vbeam_page_alloc_test.erl
erl -noshell -pa os/kernel -eval "vbeam_page_alloc_test:run()" -s init stop
```

## Integration

Use in BEAM process heap allocation:

```erlang
%% In kernel init
PageAllocState = vbeam_page_alloc:init(TotalPhysicalMemory),

%% When spawning BEAM process needing heap
{ok, HeapPages, NewState} = vbeam_page_alloc:alloc_contiguous(PageAllocState, NumPages),
%% Map HeapPages into process address space via MMU
```

## Bare Metal Notes

- NO io:format - use custom kernel logging
- NO gen_server/OTP - pure functional state passing
- NO ets - uses binary bitmap in state map
- Only stdlib basics: binary ops, lists, arithmetic
