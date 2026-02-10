# Codex Quality Review — Round 11

**Date**: 2026-02-08
**Scope**: Full codebase (vm/, kernel/, compat/)
**Findings**: 8 HIGH, 7 MEDIUM, 2 LOW = 19 total (note: 2 overlap with arch review)

## HIGH

1. **standalone:623** — decode_compact_value_recursive passes binary where decode_compact_value expects byte, so LenCode=7 operands crash with badarith. Fix: Decode one leading byte plus rest before calling decode_compact_value/2.

2. **standalone:692** — Extended allocation-list decoding accepts unbounded Count. Fix: Enforce hard max Count and verify remaining bytes.

3. **jit:25** — translate_beam reads entire input file without size limit. Fix: Check file size with file:read_file_info and reject above cap.

4. **jit:355** — Unhandled move forms silently compiled to NOP. Fix: Fail with unsupported_move error.

5. **jit:369** — Unknown external calls silently compiled to NOP. Fix: Treat as hard translation errors.

6. **bare:793** — Y-register writes have no index bound, can force huge filler-list allocation. Fix: Validate Y index and cap growth.

7. **interp:253** — Unknown opcodes logged and skipped (fail-open). Fix: Return unknown_opcode error.

8. **linuxkpi:320** — mod_timer ignores store_timer_ref failure, leaving timers untracked. Fix: Check result and cancel TRef on failure.

## MEDIUM

9. **scheduler:383** — Mailbox limits count-only not bytes. Fix: Track byte usage.
10. **scheduler:449** — Tick spoofing via unauthenticated IRQ messages. Fix: Authenticate tick source.
11. **io_server:114** — max_log_size no upper bound clamp. Fix: Strict upper bound.
12. **io_server:199** — Dynamic MFA output lacks arity/size limits. Fix: Whitelist and truncate.
13. **syscall:261** — Unknown-syscall logging prints full Args unbounded. Fix: Truncate args.
14. **parser:306** — Literal-table Count uncapped. Fix: Bound by remaining bytes.
15. **elf:686** — Unresolved symbols silently → address 0. Fix: Return error for non-weak.
16. **elf:704** — safe_nth O(n²). Fix: Use O(1) indexed structures.

## LOW

17. **scheduler:410** — PID 0 blockable via receive_message. Fix: Disallow blocking PID 0.
18. **syscall:21** — ETS init race in syscall. Fix: Handle badarg as already-initialized.
19. **linuxkpi:48** — ETS init race in linuxkpi. Fix: Same.
