# Project Rules (Claude Code)

## Session Start
- Read AI.md, LEDGER.md, PLAN.md, and HANDOFF.md to understand current state.

## While Working
- Keep diffs small and reviewable.
- Update `LEDGER.md` when decisions are made.
- Update `HANDOFF.md` when context changes or before stopping.
- All kernel code is Erlang — no C dependencies allowed.
- Machine code generation must produce verifiable binaries (test with QEMU).

## Commits
- Prefer `gg sync "message"` for commits (if gg is available).

## Key Build Commands
- `make nucleus` — build UEFI boot .efi
- `make qemu-test` — boot and verify in QEMU
- `make compile` — compile all Erlang modules
- `make test-kernel` — run kernel module tests
