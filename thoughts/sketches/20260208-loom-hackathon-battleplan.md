# Loom OS — Hackathon Battle Plan

**Contest**: Built with Opus 4.6: a Claude Code hackathon
**Dates**: Feb 10-16, 2026 (7 days)
**Target**: 1st Prize ($50,000) + "Most Creative Opus 4.6 Exploration" ($5,000)
**Team**: George + Eve (Opus 4.6)

---

## The Pitch (30 seconds)

> **Loom is an operating system where the BEAM virtual machine IS the kernel.**
>
> Linux is 34M lines of C. Buffer overflows. Kernel panics. Reboots for updates.
> Loom replaces it with BEAM: every service is an isolated process.
> Crash a driver? Supervisor restarts it. Update a module? Hot reload, zero downtime.
> The V programming language compiles to BEAM bytecode — so you write OS services in V.
>
> We built the V-on-BEAM compiler (46/46 tests, 4 compilation tiers), the UEFI boot nucleus,
> and during this hackathon: the framebuffer GUI desktop, supervisor tree, and V hello world
> running on bare metal. All orchestrated by Claude Opus 4.6.

---

## What Exists Before Hackathon (Open Source Base)

| Component | Status | LOC |
|-----------|--------|-----|
| V-to-BEAM compiler | 46/46 tests, 228/229 examples | ~10,000 |
| 4 compilation tiers | ETF, Text, Erlang Source, Direct BEAM | ~4,000 |
| UEFI boot nucleus | Boots in QEMU, serial output verified | ~300 |
| BEAM interpreter | Executes V hello world (OTP-dependent) | ~530 |
| PE emitter | Generates valid PE32+ UEFI executables | ~360 |
| x86_64 native codegen | 53 privileged instructions | ~1,200 |
| Standalone BEAM parser | Parses .beam files without OTP | ~400 |
| Linux compat layer | 450 syscalls, 66 linuxkpi shims, ELF loader | ~2,500 |
| Sokol graphics backend | wx+OpenGL on OTP | ~1,322 |
| OS architecture sketch | 5-layer design, supervisor tree, memory model | ~464 |
| Runtime modules | 28 modules (compression, audio, process, etc.) | ~3,000+ |

**Total pre-existing**: ~24,000+ LOC across compiler, runtime, OS infrastructure

---

## The Money Shot Demo (End of Hackathon)

### Scene 1: Boot (5 seconds)
```
QEMU window opens
UEFI firmware loads BOOTX64.EFI
Serial shows:
  Loom Kernel v0.2.0 booting...
  [HAL] Serial: COM1 @ 0x3F8 (115200 baud)
  [HAL] Framebuffer: 1024x768x32 via GOP
  [MEM] Page allocator: 4GB identity mapped
  [BEAM] Interpreter loaded (44 opcodes, 12 BIFs)
  [SUP] Root supervisor started (rest_for_one)
  [SUP] → vbeam_hal_sup started
  [SUP] → vbeam_console started
  [SUP] → vbeam_desktop started
  [OK] Loom ready. 0 processes, 0 crashes, 0 reboots.
```

### Scene 2: Desktop (GUI appears)
- Framebuffer shows: dark background, "Loom" logo top-left
- Taskbar at bottom: clock, process count, uptime
- Window: "V Terminal" — showing prompt
- Window: "System Monitor" — process list, supervisor tree

### Scene 3: V Hello World on Bare Metal
```v
// hello.v
fn main() {
    println('Hello from V-on-BEAM on Loom!')
}
```
Compile → runs as BEAM process → output appears in V Terminal window

### Scene 4: Fault Tolerance (THE KILLER DEMO)
- Click "Crash" button on a window → process dies
- Supervisor log: `[SUP] Process vbeam_terminal crashed (badarg)`
- Supervisor log: `[SUP] Restarting vbeam_terminal (attempt 1/5)`
- Window comes back instantly
- **"Try that on Linux."**

### Scene 5: Hot Code Reload
- Edit widget code → hot-reload command
- Widget updates LIVE without restart, no reboot
- Process state preserved
- **"Try that on Windows."**

### Scene 6: Stats
- Uptime counter shows total runtime with zero reboots
- Process count, crash count (all recovered), reload count
- **"This is what QNX promised. BEAM delivers."**

---

## Hackathon Day-by-Day Plan

### Day 1 (Feb 10): Framebuffer + Memory Manager
**Goal**: Nucleus draws to screen

- [ ] Add UEFI GOP framebuffer support to nucleus boot
  - Query `EFI_GRAPHICS_OUTPUT_PROTOCOL` for framebuffer base, width, height, stride
  - Save framebuffer info before `ExitBootServices`
  - Write pixels directly to linear framebuffer
- [ ] Basic pixel drawing: `fill_rect(x, y, w, h, color)`, `put_pixel(x, y, color)`
- [ ] Bitmap font rendering (8x16 fixed-width, embedded as binary data)
- [ ] Print "Loom" to framebuffer + serial simultaneously
- [ ] Page-frame allocator for BEAM heap (simple bitmap allocator)

### Day 2 (Feb 11): Self-Contained BEAM on Bare Metal
**Goal**: BEAM interpreter runs without OTP

- [ ] Wire standalone parser into interpreter (replace beam_disasm)
- [ ] Replace io:format with serial output + framebuffer text
- [ ] Self-contained BIF table (no erlang:apply fallback)
- [ ] Embed V-compiled hello.beam in PE data section
- [ ] Boot → parse → interpret → "Hello from V" on screen + serial
- [ ] **MILESTONE: V hello world on bare metal**

### Day 3 (Feb 12): Supervisor Tree
**Goal**: OTP-like supervision on bare metal

- [ ] Process table (pid → state, mailbox, heap)
- [ ] Simple scheduler (round-robin, reduction-counted)
- [ ] Message passing (`!` operator)
- [ ] Supervisor behaviour (one_for_one, rest_for_one)
- [ ] Root supervisor starts HAL + console + desktop

### Day 4 (Feb 13): Desktop GUI
**Goal**: Windowed desktop on framebuffer

- [ ] Window manager: rectangles with title bars
- [ ] Double buffering (draw to back buffer, flip)
- [ ] Keyboard input via UEFI protocol or PS/2
- [ ] Terminal window: displays BEAM process output
- [ ] System monitor window: process list
- [ ] Taskbar: clock, process count

### Day 5 (Feb 14): Fault Tolerance Demo
**Goal**: Crash and recover

- [ ] Crash injection: `erlang:exit(Pid, kill)` equivalent
- [ ] Supervisor restart with backoff
- [ ] Visual: window disappears, supervisor message, window comes back
- [ ] Multiple crash/recovery cycles
- [ ] "This is what makes Loom different from Linux"

### Day 6 (Feb 15): Hot Code Reload + Polish
**Goal**: Live code update + demo polish

- [ ] Code reload mechanism (replace module in interpreter)
- [ ] Demo: update widget, see it change live
- [ ] Boot sequence polish (timing, messages)
- [ ] Demo script (automated sequence showing all features)
- [ ] README.md with clear demo instructions

### Day 7 (Feb 16): Video + Submission
**Goal**: Submit

- [ ] Record demo video (screen capture of QEMU)
- [ ] Write submission essay
- [ ] Clean up code, final commit
- [ ] Submit before deadline

---

## Technical Architecture for Hackathon

### Boot Sequence (Extended)

```
UEFI Firmware
    │
    ▼
nucleus.efi (PE32+ x86_64)
    │
    ├── 1. Serial init (COM1, 115200)
    ├── 2. GOP framebuffer query
    ├── 3. ExitBootServices
    ├── 4. Identity-map 4GB (2MB pages)
    ├── 5. Init page-frame allocator
    ├── 6. Init BEAM interpreter
    │       ├── Parse embedded .beam files
    │       ├── Build code map + label map
    │       └── Init process table
    ├── 7. Start root supervisor process
    │       ├── HAL supervisor (serial, framebuffer, keyboard)
    │       ├── Console process (text output)
    │       └── Desktop process (window manager)
    └── 8. Enter scheduler loop
            └── Pick process → run reductions → preempt → repeat
```

### Framebuffer Architecture

```
GPU Framebuffer (linear, BGRA 32-bit)
    │
    ▼
Back Buffer (same size, in our heap)
    │
    ├── fill_rect(x, y, w, h, color)
    ├── draw_char(x, y, char, fg, bg)
    ├── draw_string(x, y, string, fg, bg)
    └── draw_window(x, y, w, h, title)
    │
    ▼
flip() → memcpy back → front
```

### Embedded BEAM Files

Instead of a filesystem, embed .beam binaries directly in the PE data section:

```erlang
build_boot_data() ->
    %% Embed compiled .beam files as data
    {ok, HelloBeam} = file:read_file("hello.beam"),
    {ok, ConsoleBeam} = file:read_file("vbeam_console.beam"),
    {ok, DesktopBeam} = file:read_file("vbeam_desktop.beam"),
    %% Pack as: count:32, then for each: name_len:32, name, beam_len:32, beam_data
    ...
```

---

## Why This Wins $50k

1. **Unprecedented scope**: Compiler + OS kernel + GUI desktop + reliability demos
2. **Real technical achievement**: V hello world on bare metal, not just a toy
3. **Novel**: Nobody has put BEAM on bare metal with a GUI before
4. **Claude Opus 4.6 showcase**: Orchestrating multi-agent swarms (Sonnet workers for implementation, Opus for architecture/QA) — the contest's stated focus
5. **Practical vision**: "Replace Linux" isn't just a slogan — BEAM's process model IS a better kernel
6. **Beautiful narrative**: From "Hello World" to "an OS that never crashes"

---

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| BEAM interpreter too slow | Only interpret hello.beam, not full programs |
| Framebuffer doesn't work | Fall back to serial-only demo |
| Memory manager breaks | Use static allocation (no dynamic heap) |
| Supervisor too complex | Simplified one_for_one only |
| Keyboard input fails | Use automated demo script instead |
| Time runs out | Each day has a standalone demo checkpoint |

---

## Checkpoint Demos (If We Stop Here, We Have...)

| After Day | Demo |
|-----------|------|
| Day 1 | "Loom" drawn on framebuffer + serial |
| Day 2 | V hello world on bare metal (!!!) |
| Day 3 | Multiple BEAM processes running |
| Day 4 | Windowed desktop GUI |
| Day 5 | Crash-and-recover demo |
| Day 6 | Hot code reload + polished demo |
| Day 7 | Submitted with video |

Each checkpoint is independently impressive. Even Day 2 alone would be remarkable.
