# Codex Architecture Review — Round 14

**Date**: 2026-02-10
**Findings**: 3 CRITICAL, 2 HIGH = 5 total

## CRITICAL
1. **scheduler:485** — IRQ path still spoofable when bridge PID unset.
2. **io_server:195** — MFA whitelist still too broad (lists/*, unicode/*).
3. **bare:159** — Invalid import index fallback crashes on safe_to_atom(undefined).

## HIGH
4. **native:361** — string__int emits {mul,...,{imm,10}} but lowerers only handle register mul.
5. **native:601** — Builtins emit malformed store IR operand order.
