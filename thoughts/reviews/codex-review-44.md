# Codex Review — Round 44

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 7 HIGH, 6 MEDIUM, 1 LOW = 17 total

## Architecture (7 findings)

### CRITICAL
1. **arm64_lower:221/250, link:208/259** — ARM64 div-by-zero guard relocs emitted as x86 `rel32` instead of `arm64_cond_branch19`; linker overwrites entire ARM64 instruction word.
2. **x86_lower:1089/1092, x86_enc:56/431** — `map_put` growth calls non-existent `encode_shl_imm/2`; crashes with `undef`.
3. **x86_lower:1093, alloc:177/306** — `map_put` passes register atom to `emit_alloc/3` expecting integer; crashes with `badarith`.

### HIGH
4. **x86_lower:1093, alloc:179/184/192** — Map growth allocates into `r14` (heap-end sentinel); corrupts bounds checks.
5. **x86_lower:1088/1091, regalloc:491/492/353/571** — `map_put` growth clobbers `r12/r13` but not treated as clobber barrier; live values silently corrupted.
6. **link/native:93/124/131/145** — BSS entries ignored, `BssSize=0` hardcoded; BSS symbols unresolved at link time.

### MEDIUM
7. **x86_lower:762/779/809/838** — Array OOB exits hardcode Linux syscall 60 despite computing format-specific `SysExit`.

## Quality (10 findings)

### HIGH
8. **irq_bridge:296/326/404/415** — Token state not cleared on handler unregister/death; stale tokens inherited.
9. **irq_bridge:169/171, scheduler:523/542** — `ack_irq` TOCTOU race: `whereis` + `gen_server:call` can crash if bridge dies between calls.
10. **scheduler:533/535/540** — Legacy IRQ path accepts unauthenticated `{irq,32,...}` when `allow_legacy_irq` enabled.
11. **linuxkpi:55/574/582/597** — Timer ETS `protected` but caller processes write directly; non-owner writes crash with `badarg`.

### MEDIUM
12. **linux_syscall:50/53/178/184** — Global catch swallows `exit`/`throw` including intended termination (`sys_exit`/`sys_exit_group`).
13. **beam_parser:259-366, beam_standalone:453** — Parser fallbacks still return partial tables on truncation.
14. **beam_interp_bare:749/752/799** — `~c` format with non-byte arg crashes interpreter (DoS).
15. **test_elf_loader_bounds:60/62/95** — Tests simulate bounds checks on `undef` instead of exercising real loader.
16. **scheduler_test:8/136, irq_bridge_test:13** — Tests only exercise legacy IRQ path, never token-authenticated path.

### LOW
17. **test_decode_verification:53** — Decoder failure downgraded to partial success in tests.
