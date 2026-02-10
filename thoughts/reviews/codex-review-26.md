# Codex Review — Round 26

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 7 HIGH, 2 MEDIUM = 11 total (0 repeats filtered)

## CRITICAL
1. **x86_64:404/23/89** — ret emits epilogue(0) when no callee-saved regs, but FrameSize can be non-zero. Returns with rsp in spill space, reading wrong return address.
2. **jit:457/486** — translate_serial_output does lodsb from [rsi], but translate_put_string clears RSI (xor rsi,rsi). Dereferences address 0.

## HIGH
3. **x86_64:310/328/345** — Shift lowering when Dst=rcx, B=rcx: mov rcx,A clobbers shift count in cl before shl rcx,cl.
4. **x86_64:253/261/278** — sdiv/srem remaps divisor to r11, but if dividend A==r11, SaveB clobbers A before MovA.
5. **native:195/198** — find_entry fallback (no main) recursively sums function sizes instead of returning 0; produces end-of-text offset.
6. **page_alloc:208/104** — mark_reserved_pages collapses to single reserved_end high-water mark; free_page_checked blocks freeing ANY page <= reserved_end.
7. **irq_bridge:300/371** — pending_counts not cleared on handler unregister or death; stale counts permanently throttle IRQs.
8. **elf_loader:587/648** — Relocations keyed by section name not index; duplicate section names cause misapplication.
9. **boot_sequence:134/153/175/197** — No overlap/spacing validation; too-close addresses silently clamp padding to zero, diverging offsets from load addresses.

## MEDIUM
10. **irq_bridge:351/160** — ack_irq doesn't authenticate caller; any process can spoof ACKs and bypass backpressure.
11. **linuxkpi:542/559/567** — Timer refs keyed by {self(), Timer}; cross-process cancel/rearm fails, leaving stale active timers.
