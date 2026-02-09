# Loom OS Demo Video Script

**Target length:** 2-3 minutes
**Tone:** Technical but accessible. Confident, not hype-y.

---

## Opening (10s)

**Visual:** Boot splash in QEMU — dark purple background, loom grid, "LOOM OS v0.2.0"

**Narration:**
> This is Loom OS — an operating system where the BEAM virtual machine is the kernel. Written in V, compiled to BEAM bytecode, running on bare metal.

---

## Act 1: The Compiler (30s)

**Visual:** Terminal showing V source code, then compilation

**Narration:**
> V is a simple, fast programming language. Our compiler translates V to BEAM bytecode through four different compilation tiers — all producing identical output. 228 out of 229 V examples compile successfully. That's 99% compatibility.

**Show:**
```bash
cat hello.v        # Show the V source
v -b beam hello.v  # Compile it
erl -noshell ...   # Run it -> "Hello from V on BEAM!"
```

---

## Act 2: The Boot (30s)

**Visual:** QEMU window showing boot splash + serial console

**Narration:**
> The nucleus is a 5-kilobyte UEFI binary generated entirely by Erlang — byte by byte, no assembler, no external toolchain. It boots from firmware, initializes the serial console and graphics framebuffer, and then executes V code that was compiled to native x86_64 through our BEAM-to-native translator.

**Show:** QEMU boot with serial output scrolling

---

## Act 3: Fault Tolerance (30s)

**Visual:** Terminal showing crash/restart demo

**Narration:**
> Here's why BEAM makes a great kernel. Watch what happens when a process crashes. [crash happens] The supervisor detected the crash and restarted the process in under a millisecond. The rest of the system never noticed. In Linux, a driver crash means a kernel panic and a reboot. In Loom OS, it's an event the supervisor handles automatically.

**Show:** `./demos/fault_tolerance/run_demo.sh`

---

## Act 4: Hot Code Reload (30s)

**Visual:** Terminal showing live code swap

**Narration:**
> Now watch this. We're going to update running code — without stopping the process. Same PID, same state, new behavior. This is hot code reloading, and it's built into BEAM at the VM level. Imagine updating a device driver on a production server without any downtime.

**Show:** `./demos/hot_reload/run_demo.sh`

---

## Act 5: The Numbers (20s)

**Visual:** Project stats overlay or terminal output

**Narration:**
> The V-on-BEAM compiler has 46 runtime tests, all passing. 28 Erlang runtime modules. A native backend that compiles 68 out of 68 V examples to ARM64. 53 privileged x86_64 instructions for kernel-level operations. Linux syscall compatibility for 450 system calls. All orchestrated by Claude Opus 4.6 across 30+ collaborative sessions.

---

## Closing (10s)

**Visual:** Boot splash again, or the loom grid

**Narration:**
> Loom OS. Threads woven together. Built with Opus 4.6.

---

## Recording Notes

- Use a clean terminal with dark background
- Font size large enough to read on video
- Record QEMU in a window, not fullscreen (so judges can see it's real)
- Serial console output is the proof — make sure it's visible
- Keep terminal commands visible for 2-3 seconds before pressing enter
