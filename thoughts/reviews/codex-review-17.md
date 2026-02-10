# Codex Review — Round 17

**Date**: 2026-02-10
**Findings**: 3 HIGH = 3 total

## HIGH (all interpreter call semantics)
1. **interp_v2:200** — Local call only matches {f,Label}, but beam_disasm emits {Module,Function,Arity}.
2. **interp_v2:228** — call_only tail call resets y to [], wiping caller frames.
3. **bare:516** — Same y=>[] wipe in handle_local_call.
