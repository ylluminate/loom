# Sketch: Reserve heap register in regalloc when data types are used

See primary sketch: thoughts/sketches/20260206-1655-phase6-native-data-types.md

COVERS:
- vbeam_rt/src/vbeam_native_regalloc.erl

## Current State

```mermaid
flowchart TD
    Body[IR Body] --> LiveIntervals[compute_live_intervals]
    LiveIntervals --> ParamAssign[param_assignments]
    ParamAssign --> CallAware[available_regs_call_aware]
    CallAware --> AllRegs["All available regs<br/>ARM64: x0-x18, x19-x28<br/>x86: rax-r15"]
    AllRegs --> FreePool[Remove param regs]
    FreePool --> LinearScan[linear_scan allocation]
    LinearScan --> Rewrite[rewrite_instructions]
```

## What I'm Changing

When `reserve_heap` option is set (because string/array/struct/map ops exist):
- Remove x28 (ARM64) or r15 (x86_64) from the available register pool
- This prevents the allocator from assigning these regs to user variables
- The heap reg is used by alloc_init/alloc code emitted during lowering

```mermaid
flowchart TD
    Body[IR Body] --> NeedsHeap{needs_heap?}
    NeedsHeap -->|yes| ReserveHeap["Opts = {reserve_heap: true}"]
    NeedsHeap -->|no| NoReserve["Opts = {}"]
    ReserveHeap --> Filter["AllRegs = AllRegs -- [x28/r15]"]
    NoReserve --> NoFilter["AllRegs unchanged"]
    Filter --> LinearScan[linear_scan]
    NoFilter --> LinearScan
```

## What Must NOT Break
- Existing programs without heap ops get the same register pool as before
- x28/r15 are only reserved when heap ops are actually present

## How I'll Verify It Works
- [ ] hello_arm64.ir still compiles and runs (no heap ops, x28 available)
- [ ] strings_arm64.ir compiles without x28 conflict
