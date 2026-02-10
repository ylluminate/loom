# Codex Quality Review — Round 12

**Date**: 2026-02-10
**Scope**: vm/, kernel/, compat/
**Findings**: 1 HIGH, 3 MEDIUM = 4 total (down from 19 in R11!)

## HIGH

1. **jit:388** — Serial-output loop reads from RSI until NUL with no bounds check. Fix: Emit length-bounded writes and validate pointers.

## MEDIUM

2. **standalone:581** — Malformed compact values coerced to 0 instead of decode failure. Fix: Propagate errors.

3. **jit:122** — Translation pipeline has no instruction/code-size ceilings. Fix: Add hard caps.

4. **jit:132** — Decode/translation failures in translate_chunks not isolated. Fix: Wrap in try/catch.
