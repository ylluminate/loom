# Codex Architecture Review — Round 12

**Date**: 2026-02-10
**Scope**: Full codebase
**Findings**: 1 CRITICAL, 3 HIGH = 5 total (down from 17 in R11!)

## CRITICAL

1. **elf:729** — find_init_addr/extract_exports use BaseAddr + st_value for internal symbols, ignoring symbol section base. Fix: Compute VA as section_base + st_value.

## HIGH

2. **elf:194** — Section placement ignores per-section sh_addralign, forces 16-byte alignment. Fix: Align each section by its own addralign.

3. **syscall:219** — sys_arch_prctl returns success for ARCH_GET_FS/GS without tracking FS/GS bases. Fix: Maintain per-thread FS/GS state.

4. **kpi_symbols:104** — request_threaded_irq mapped to request_irq/5, dropping thread handler (Linux ABI is 6 args). Fix: 6-arg shim.

5. **kpi_symbols:46** — dma_free_coherent mapped to arity 3 (Linux ABI is 4). Fix: /4 implementation.
