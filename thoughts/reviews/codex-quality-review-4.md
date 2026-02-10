**Baseline**
1. `make compile` passed.
2. `make check` passed (`33 passed`).
3. `make test` passed (all kernel/vm suites green).
4. `make info` reports `33` source modules and `15` test modules (plus test data files).

**Findings (new issues)**
1. `HIGH` — `vm/parser/vbeam_beam_parser.erl:46` and `vm/parser/vbeam_beam_standalone.erl:77` use `binary_to_atom` on parsed file data. This can atom-table DoS on crafted input. Fix: whitelist chunk IDs and use safe mapping (or `binary_to_existing_atom` with controlled preloaded atoms).
2. `HIGH` — `vm/parser/vbeam_beam_parser.erl:142`/`149`/`156`/`218` and `vm/parser/vbeam_beam_standalone.erl:174`/`180`/`186`/`266` use `binary_to_term` without `[safe]`. Crafted BEAM chunks can allocate unbounded terms/atoms. Fix: `binary_to_term(Bin, [safe])`, strict size caps, and explicit error returns.
3. `HIGH` — `vm/parser/vbeam_beam_standalone.erl:424` computes medium compact-term values incorrectly (`(Byte band 2#11100000) bsl 3`). This decodes many operands to wrong numbers. Fix formula to `(Byte bsr 5) bor (Next bsl 3)` and add boundary tests.
4. `HIGH` — `kernel/io/vbeam_irq_bridge.erl:232`-`237` + `300`-`309`: re-registering the same IRQ leaves stale monitor refs; when old PID dies, `DOWN` removes the new handler. Reproduced: handler map becomes empty after killing old PID. Fix: on re-register, demonitor/remove existing IRQ monitor first; maintain irq↔monitor bijection.
5. `HIGH` — `vm/interp/vbeam_beam_interp_v2.erl:231`-`237`: `call_ext_only` returns from the whole VM loop instead of returning to caller frame. Reproduced with synthetic nested call; interpreter returns early. Fix: unwind one frame (normal return path), don’t emit top-level `{return, ...}` directly.
6. `HIGH` — `vm/interp/vbeam_beam_interp_v2.erl:171`-`179`: normal return forcibly sets `y = []`, destroying caller frame locals. Reproduced crash path at deallocate after call. Fix: restore caller frame, do not zero `y` on ordinary return.
7. `HIGH` — `vm/interp/vbeam_beam_interp_bare.erl:524`-`527`: same `call_ext_only` premature-global-return bug exists in bare interpreter. Fix mirrors v2.
8. `HIGH` — `vm/interp/vbeam_beam_interp_bare.erl:257`-`268`: same return-path `y => []` frame-loss bug in bare interpreter. Fix mirrors v2.
9. `HIGH` — `vm/jit/vbeam_beam_to_native.erl:109` plus opcode clauses at `174`-`237`: real decoded instructions are shape `{op, [Operands...]}`, but translator patterns mostly expect normalized forms (`return`, `{allocate,Int,...}`), so real BEAM translation falls through to NOPs. Fix: normalize decoded instructions first (like bare interpreter) or update translator patterns to actual decoded AST.
10. `HIGH` — `vm/jit/vbeam_beam_to_native.erl:187`-`191` and `193`-`197`: stack deltas are encoded as 8-bit immediates; large frames truncate silently (`40*8 -> 64` encoded). Fix: use imm8/imm32 selection with bounds checks.
11. `HIGH` — `vm/jit/vbeam_beam_to_native.erl:331`: inline serial-output expansion ends with `ret`, which can return from the current function mid-body. Fix: remove inline `ret` and either fall through or call helper with proper reloc.
12. `HIGH` — `compat/elf/vbeam_elf_loader.erl:182`-`192` computes aligned section virtual addrs, but `208`-`212` concatenates sections without padding. Relocations can target wrong bytes. Fix: emit sections at aligned offsets (with explicit padding) consistent with address assignment.
13. `MEDIUM` — `compat/elf/vbeam_elf_loader.erl:522`-`533` truncates relocation values via `band 16#FFFFFFFF` without signed/unsigned range checks. Overflow is silent corruption. Fix: validate per relocation type (`PC32`, `32`, `32S`) and error on out-of-range.
14. `HIGH` — `kernel/mm/vbeam_page_alloc.erl:83`-`98`: reserved pages can be “freed” (e.g. physical `0`), corrupting reservation accounting and exposing kernel-reserved memory. Fix: track reserved vs allocated separately or reject freeing reserved region.
15. `HIGH` — `kernel/mm/vbeam_page_alloc.erl:247` (via `free_page`): out-of-range address causes pattern-match crash (`badmatch`) instead of safe error/ignore. Fix: bounds-check page index before bit access.
16. `HIGH` — `compat/syscall/vbeam_linux_syscall.erl:28` dispatches to strict-arity handlers (e.g. `:103`) without argument validation; malformed args crash with `function_clause` instead of returning `EINVAL`. Fix: validate argument list lengths/types before handler calls.
17. `MEDIUM` — `compat/kpi/vbeam_linuxkpi.erl:273`-`279` and `282`-`285`: timers send to caller `self()` and refs aren’t stored/cancelled, so `del_timer` is ineffective and semantics are wrong. Fix: central timer registry keyed by timer object, cancel old refs on `mod_timer`, real `del_timer`.
18. `MEDIUM` — `kernel/io/vbeam_io_server.erl:308`-`315`: wrap-on-width increments `Y` without height clamp/scroll (newline path clamps, wrap path doesn’t). Fix: apply same clamp/scroll logic in both paths.
19. `MEDIUM` — `kernel/sched/vbeam_scheduler.erl:88` docs include `max_processes`, but spawn path `269`-`305` never enforces it, enabling unbounded growth. Fix: reject spawns past configured limit.

**Test gaps**
1. `HIGH` — `tests/kernel/vbeam_beam_to_native_test.erl:95`-`177` tests `translate_function/2` with handcrafted ops, but not real `translate_beam/1` decode path; this misses the instruction-shape mismatch bug.
2. `HIGH` — `tests/vm/test_bare_pipeline.erl:245`-`288` verifies unknown-opcode handling by source-string search, not runtime behavior.
3. `HIGH` — `tests/vm/test_elf_loader_bounds.erl:57`-`72` and `95`-`107` mostly simulate checks instead of exercising real relocation patching path.
4. `MEDIUM` — `tests/kernel/vbeam_irq_bridge_test.erl:119`-`137` / `249`-`280` lacks “re-register same IRQ then old handler dies” regression test.
5. `MEDIUM` — `tests/kernel/vbeam_paging_test.erl:278`-`287` explicitly skips the negative 2MB-alignment assertion, leaving that path unprotected.
6. `MEDIUM` — `tests/vm/test_native_stubs.erl:68`-`83` and `136`-`143` only checks compile/non-empty binary, not actual register semantics (`rax` vs `x0`).

Unexpected workspace metadata changes also appeared during runs: `.preflight-cache/checkpoint.json`, `.preflight-result.json`, and `.preflight-cache/sessions/preflight-20260210-024439-993a7553/`. I did not edit source files.