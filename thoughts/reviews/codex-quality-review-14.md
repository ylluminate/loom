# Codex Quality Review — Round 14

**Date**: 2026-02-10
**Findings**: 5 MEDIUM

## MEDIUM
1. **interp_v2:586** — BIF allowlist includes higher-order lists APIs (map, filter, foldl).
2. **interp:490** — Arity from import metadata unbounded, used in lists:seq.
3. **standalone:189** — LitT decompression failure falls back to raw bytes (fail-open).
4. **linuxkpi:293** — request_irq returns success without registering handler.
5. **syscall:235** — ARCH_GET_FS/GS stores under user-controlled keys, unbounded dict growth.
