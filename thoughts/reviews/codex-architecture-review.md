**Findings (Ordered by Severity)**
1. **Bare-metal readiness is overstated in core VM path.** `vbeam_beam_standalone` still uses `zlib:uncompress` and `binary_to_term` (`vm/parser/vbeam_beam_standalone.erl:141`, `vm/parser/vbeam_beam_standalone.erl:174`, `vm/parser/vbeam_beam_standalone.erl:266`), and file I/O (`vm/parser/vbeam_beam_standalone.erl:27`). So it is not “zero OTP deps” in practice.
2. **`vbeam_beam_interp_bare` has hidden transitive dependency on parser/runtime services.** It calls `vbeam_beam_standalone:parse_binary/1` and `decode_instructions/2` (`vm/interp/vbeam_beam_interp_bare.erl:44`, `vm/interp/vbeam_beam_interp_bare.erl:49`), so parser constraints flow into the bare interpreter.
3. **Build/test targets can report green while quality is red.** `make compile` suppresses compiler stderr and does not fail the target on per-module compile failures (`Makefile:108`, `Makefile:110`). `make test-kernel` / `make test-vm` call `mod:test()` (`Makefile:171`, `Makefile:184`) but many test modules export `run/0` or `run_all/0` (for example `tests/kernel/vbeam_page_alloc_test.erl:7`, `tests/kernel/vbeam_beam_to_native_test.erl:3`), causing systemic false failures and unreliable status.
4. **Compat syscall layer is structurally incomplete in this repo.** `vbeam_linux_syscall` calls non-present modules (`vbeam_vfs`, `vbeam_vmm`, `vbeam_proc_table`, `vbeam_signal`, `vbeam_time`) (`compat/syscall/vbeam_linux_syscall.erl:104`, `compat/syscall/vbeam_linux_syscall.erl:123`, `compat/syscall/vbeam_linux_syscall.erl:136`, `compat/syscall/vbeam_linux_syscall.erl:174`, `compat/syscall/vbeam_linux_syscall.erl:210`).
5. **Documentation drift is significant.** Claims of “zero OTP deps” and “36/36 modules” are out of date/inaccurate (`README.md:113`, `LEDGER.md:14`, `LEDGER.md:69`, `LEDGER.md:84`, `HANDOFF.md:19`, `HANDOFF.md:41`, `HANDOFF.md:42`).

**Dependency Summary**
- Source modules analyzed: **33**
- Explicit `-import(...)` usage: **none**
- Internal module-call edges: **19**
- Circular dependencies: **none detected** (acyclic internal graph)

**Module Dependencies (Imports/Calls)**
| Module | Internal calls | External calls |
|---|---|---|
| `vbeam_native_arm64` | — | `lists` |
| `vbeam_native_lower_arm64` | `vbeam_native_alloc` | `erlang`, `io`, `lists`, `maps` |
| `vbeam_native_elf` | — | — |
| `vbeam_native_macho` | — | — |
| `vbeam_native_pe` | — | — |
| `vbeam_native_alloc` | — | `lists` |
| `vbeam_native_ir` | — | `file`, `io_lib`, `lists` |
| `vbeam_native_regalloc` | `vbeam_native_alloc` | `lists`, `maps` |
| `vbeam_native` | `vbeam_native_alloc`, `vbeam_native_elf`, `vbeam_native_ir`, `vbeam_native_link`, `vbeam_native_macho`, `vbeam_native_pe`, `vbeam_native_regalloc` | `file`, `init`, `io`, `lists`, `maps`, `os`, `sets` |
| `vbeam_native_link` | — | `lists`, `maps` |
| `vbeam_native_lower_x86_64` | `vbeam_native_alloc` | `erlang`, `io`, `lists` |
| `vbeam_native_x86_64` | — | — |
| `vbeam_font_8x16` | — | `binary` |
| `vbeam_font_monaspice` | — | `binary` |
| `vbeam_nucleus_boot` | `vbeam_font_8x16`, `vbeam_native_pe` | `file`, `filename`, `io` |
| `vbeam_elf_loader` | — | `file`, `lists`, `maps` |
| `vbeam_kapi_symbols` | — | `logger`, `maps` |
| `vbeam_linuxkpi` | — | `erlang`, `io_lib`, `logger` |
| `vbeam_linux_syscall` | — | `io`, `io_lib`, `persistent_term`, `sets`, `vbeam_proc_table`, `vbeam_signal`, `vbeam_time`, `vbeam_vfs`, `vbeam_vmm` |
| `vbeam_gdt_idt` | — | `binary`, `lists` |
| `vbeam_boot_sequence` | `vbeam_gdt_idt`, `vbeam_paging` | `lists`, `maps` |
| `vbeam_io_server` | — | `gen_server`, `io`, `maps`, `unicode` |
| `vbeam_irq_bridge` | — | `erlang`, `gen_server`, `lists`, `maps` |
| `vbeam_heap` | `vbeam_page_alloc` | `erlang`, `maps` |
| `vbeam_page_alloc` | — | `lists` |
| `vbeam_paging` | — | `lists` |
| `vbeam_scheduler` | `vbeam_heap`, `vbeam_page_alloc` | `erlang`, `gen_server`, `lists`, `maps`, `queue` |
| `vbeam_beam_interp` | — | `io`, `lists`, `maps`, `proplists` |
| `vbeam_beam_interp_bare` | `vbeam_beam_standalone` | `maps` |
| `vbeam_beam_interp_v2` | — | `beam_disasm`, `io`, `lists`, `maps`, `proplists` |
| `vbeam_beam_to_native` | `vbeam_beam_standalone` | `file`, `lists`, `maps` |
| `vbeam_beam_parser` | — | `file`, `lists`, `zlib` |
| `vbeam_beam_standalone` | — | `file`, `lists`, `maps`, `zlib` |

**Bare-Metal Readiness Matrix**
Legend: `YES` = runnable with minimal BEAM core assumptions, `PARTIAL` = close but needs adaptation, `NO` = hard OTP/host/runtime blockers.

| Module | Status | Reason |
|---|---|---|
| `vbeam_native_arm64` | YES | pure encoder style |
| `vbeam_native_lower_arm64` | PARTIAL | `io:format` warning path |
| `vbeam_native_elf` | YES | pure emitter |
| `vbeam_native_macho` | YES | pure emitter |
| `vbeam_native_pe` | YES | pure emitter |
| `vbeam_native_alloc` | YES | pure lowering helper |
| `vbeam_native_ir` | PARTIAL | `file:consult` / `file:write_file` helpers |
| `vbeam_native_regalloc` | YES | pure allocation logic |
| `vbeam_native` | NO | host CLI/file/os/init responsibilities |
| `vbeam_native_link` | YES | pure linker state/patch logic |
| `vbeam_native_lower_x86_64` | PARTIAL | `io:format` warning path |
| `vbeam_native_x86_64` | YES | pure encoder |
| `vbeam_font_8x16` | YES | static binary/font helpers |
| `vbeam_font_monaspice` | YES | static binary/font helpers |
| `vbeam_nucleus_boot` | NO | build-time file I/O/logging |
| `vbeam_elf_loader` | PARTIAL | parse logic is pure; `load/2` uses file I/O |
| `vbeam_kapi_symbols` | NO | `logger` dependency |
| `vbeam_linuxkpi` | NO | `logger` dependency surface |
| `vbeam_linux_syscall` | NO | `persistent_term` + missing service modules |
| `vbeam_gdt_idt` | YES | pure binary generation |
| `vbeam_boot_sequence` | YES | pure binary/layout generation |
| `vbeam_io_server` | NO | `gen_server`/`io`/unicode OTP path |
| `vbeam_irq_bridge` | NO | `gen_server` dependency |
| `vbeam_heap` | YES | pure state transitions |
| `vbeam_page_alloc` | YES | pure allocator logic |
| `vbeam_paging` | YES | pure page table generation |
| `vbeam_scheduler` | NO | `gen_server`/`queue` runtime dependency |
| `vbeam_beam_interp` | NO | legacy interpreter uses `io`/host-style assumptions |
| `vbeam_beam_interp_bare` | PARTIAL | core loop is clean, but parser dependency is not |
| `vbeam_beam_interp_v2` | NO | `beam_disasm` dependency |
| `vbeam_beam_to_native` | NO | file I/O + parser dependency |
| `vbeam_beam_parser` | NO | `file` + `zlib` + `binary_to_term` |
| `vbeam_beam_standalone` | NO | `file` + `zlib` + `binary_to_term` |

**Focused Checks You Requested**
- `vm/parser/vbeam_beam_standalone.erl`: uses `zlib:uncompress` and `binary_to_term` as expected (`vm/parser/vbeam_beam_standalone.erl:141`, `vm/parser/vbeam_beam_standalone.erl:174`, `vm/parser/vbeam_beam_standalone.erl:266`).
- `vm/interp/vbeam_beam_interp_bare.erl`: no direct `gen_server`/`file`/`io` calls, but hidden transitive parser dependency (`vm/interp/vbeam_beam_interp_bare.erl:44`, `vm/interp/vbeam_beam_interp_bare.erl:49`).
- `vm/jit/vbeam_beam_to_native.erl`: rewired off `beam_lib`/`beam_disasm`; now uses standalone parser (`vm/jit/vbeam_beam_to_native.erl:27`, `vm/jit/vbeam_beam_to_native.erl:109`) but still uses host file reads (`vm/jit/vbeam_beam_to_native.erl:25`).

**Naming Conventions**
- Strong overall consistency: all production modules use `vbeam_*`; file/module names match.
- Notable inconsistency: `compat/kpi/` folder with module `vbeam_kapi_symbols` (`compat/kpi/vbeam_kapi_symbols.erl:1`) mixes `kpi` vs `kapi` naming.
- Versioned interpreter names are understandable (`vbeam_beam_interp`, `_v2`, `_bare`).

**API Surface Review**
- Mostly explicit exports with reasonable boundaries.
- `vbeam_io_server` exports gen_server callbacks but lacks `-behaviour(gen_server)` declaration (callbacks are exported at `kernel/io/vbeam_io_server.erl:22`; behavior declaration missing at top).
- Some surfaces look broader than needed for production API (for example ring internals in `vbeam_irq_bridge` are public).
- Native encoder modules intentionally have wide export surfaces (instruction encoders), which is coherent for a codegen library.

**Test Coverage**
- Directly covered critical modules: `vbeam_heap`, `vbeam_page_alloc`, `vbeam_paging`, `vbeam_scheduler`, `vbeam_io_server`, `vbeam_irq_bridge`, `vbeam_gdt_idt`, `vbeam_boot_sequence`, `vbeam_beam_to_native`, `vbeam_beam_interp_bare`, `vbeam_beam_standalone`.
- Critical modules lacking direct tests: `vbeam_beam_interp_v2`, `vbeam_beam_parser`, `vbeam_nucleus_boot`, all major `arch/*` pipeline modules (`vbeam_native*`), `vbeam_elf_loader`, `vbeam_linux_syscall`, `vbeam_linuxkpi`, `vbeam_kapi_symbols`.
- Current harness issue: Makefile expects `test/0` but many test modules export `run/0` or `run_all/0`, so results are not trustworthy as a gate (`Makefile:171`, `Makefile:184`).

**Directory Structure Review**
- `boot/kernel/vm/arch/compat` split is conceptually solid for OS architecture.
- Good separation of tests into `tests/`.
- Main ambiguity is dual “arch” meanings: `kernel/arch` (runtime boot arch setup) vs top-level `arch` (compiler/backend/toolchain). This is workable but should be explicitly documented.
- `compat/syscall` currently points to service modules not present in repo, indicating architectural placeholders rather than integrated implementation.

**Build System Review (Makefile)**
- Good: clear targets, subsystem compile targets, help/info UX.
- Problem: compile/test targets are not fail-fast and hide diagnostics (`2>/dev/null` in compile/test loops).
- Problem: test entrypoint mismatch (`test/0` hardcoded) vs actual exported test functions.
- Problem: `make check` is strict and currently reports **12 failing modules** (warnings-as-errors), while `make compile` still reports success.
- Problem: beam count in `_build/` is inflated by stale test artifacts (reported 47 while source module count is 33).

**Documentation Accuracy (`README.md`, `LEDGER.md`, `HANDOFF.md`)**
- Inaccurate “zero OTP dependency” claims for standalone parser/bare pipeline (`README.md:113`, `LEDGER.md:14`, `LEDGER.md:84`, `HANDOFF.md:41`, `HANDOFF.md:42`).
- Outdated module-count claims (`LEDGER.md:69`, `HANDOFF.md:19`).
- `README` says “run all tests: make test” (`README.md:145`), but Makefile `test` only runs kernel+vm, not native test harness.

**Validation Run Notes**
- Ran: `make compile`, `make test-kernel`, `make test-vm`, `make check`, `make info`.
- `make check` result: **21 passed / 12 failed** (warning-as-error failures).
- Preflight script exists but failed in this environment due missing persona configuration (no review personas configured).

1. If you want, I can turn this into a concrete remediation plan with priority order and expected effort.
2. I can also patch just the test/build harness first (`Makefile` + test module entrypoint normalization) so CI truthfulness is fixed before deeper bare-metal work.