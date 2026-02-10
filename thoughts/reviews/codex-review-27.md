# Codex Review — Round 27

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 5 HIGH, 3 MEDIUM = 10 total (0 repeats filtered)

## CRITICAL
1. **jit:347/470/248** — call_ext_last not terminal; translate_serial_output falls through. Functions concatenated, so terminal call_ext_last executes into next function's bytes.
2. **gdt_idt:346/353/356** — build_timer_stub writes counter to hardcoded physical address 0x7000 (unowned); periodic IRQs corrupt arbitrary kernel data.

## HIGH
3. **x86_64:323/344/363/253/286, regalloc:426/472** — r11 used as implicit scratch in shift/div but allocator freely assigns r11 to live values; silent clobber.
4. **x86_64:22/1675/1735/232/454, native:121** — x86 lowerer recomputes callee-saved with partial extract_regs instead of using regalloc's used_callee_saved; missed variants drop saves.
5. **irq_bridge:424** — Handler replacement doesn't reset pending_counts; new handler inherits saturated count, permanently throttled.
6. **linuxkpi:143** — printk uses io_lib:format with Linux-style format strings; "%s" triggers badarg crash.
7. **interp_bare:825/347** — deallocate_stack calls error() on invalid input; callers don't catch → interpreter process terminates.

## MEDIUM
8. **interp:491/513** — execute_bif malformed nested case; import entry errors cause case_clause crash instead of structured error.
9. **linuxkpi:401** — del_timer only handles {TRef, Gen} format; legacy bare refs hit case_clause crash.
10. **interp_v2:265/276** — call_ext builds arg list with lists:seq without arity bounds check; large arity causes memory blowup.
