# Codex Review — Round 31

**Date**: 2026-02-10
**Findings**: 2 CRITICAL, 11 HIGH = 13 total (0 repeats filtered)

## CRITICAL
1. **alloc:140/163/164, arm64:647/915** — `emit_alloc(arm64, DstReg, ...)` hardcodes `x16` as temp; when `DstReg = x16` (array_new/map_new), new pointer is overwritten before commit → heap never advances, allocations overlap.
2. **x86_64:701/745/770/852, arm64:687/750/785/863** — Array element access always uses 64-bit load/store regardless of ElemSize; for ElemSize < 8, reads/writes past element boundaries → adjacent memory corruption.

## HIGH
3. **regalloc:550/552/571/577** — Spill accounting counts operand occurrences not unique vregs; repeated use of same spilled vreg inflates count → false `too_many_spills_in_instruction` crash.
4. **regalloc:323/328/329, x86_64:1271/1279** — `float_to_str` not treated as call barrier; x86 expands it to `int_to_str` which clobbers caller-saved regs → silent register corruption.
5. **native:156, x86_64:19/690/693/739/741/760/762** — x86 lowering ignores Format, hardcodes Linux sys_exit in OOB paths; no ud2/int3 after fatal syscall → fall-through into unchecked memory access.
6. **page_alloc:86/291** — `alloc_pages/2` accepts negative Count; `alloc_pages_loop/3` only terminates at 0 → infinite allocation loop.
7. **page_alloc:182/197** — `alloc_contiguous/2` no Count > 0 guard; Count=0 gives bogus success, Total=0 → badarith in modulo.
8. **page_alloc:50/59** — `init/1` no validation for negative TotalMemoryBytes → negative BitmapSize → badarg in binary construction.
9. **boot_sequence:52/65/345** — `boot_code/1` assumes paging returns binary; `{error,...}` tuple → crash in `strip_trailing_ret/1`.
10. **elf_loader:421** — `.shstrtab` lookup via `element(ShStrNdx+1, ...)` with no bounds check → badarg.
11. **linux_syscall:338/360/361** — Stub logging fully materializes args before truncation → heavy allocation/system_limit for large terms.
12. **interp_v2:634/639/680/684** — BIF allowlist includes `lists:seq/2`, `lists:duplicate/2` with no argument caps → unbounded allocation.
13. **linuxkpi:313/318/328** — `request_irq/5` and `request_threaded_irq/6` return success without registering → drivers hang waiting for IRQs.
