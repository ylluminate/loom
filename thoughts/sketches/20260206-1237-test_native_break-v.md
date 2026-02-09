# Sketch: Add break/continue test for native IR backend

COVERS:
- test_native_break.v (NEW test file)
- irgen.v (already has break/continue support, just needs testing)

## Current State
```mermaid
flowchart TD
    A[V Source .v] --> B[irgen.v: gen_stmt]
    B --> C{Statement type?}
    C -->|ForCStmt| D[gen_for_c]
    C -->|ForStmt| E[gen_for]
    C -->|BranchStmt| F[loop_stack lookup]
    D --> G[push loop_stack: continue_lbl, break_lbl]
    E --> G
    F --> H{kind?}
    H -->|break| I[jmp break_lbl]
    H -->|continue| J[jmp continue_lbl]
    G --> K[gen body stmts]
    K --> L[pop loop_stack]
    L --> M[IR output: labels + jmps]
    M --> N[vbeam_native: assemble_parts]
    N --> O[scope labels per function: fn@@label]
    O --> P[linker resolves branches]
    P --> Q[ARM64 binary]
```

## What I'm Changing
Adding a new test file `test_native_break.v` that exercises:
1. `break` in infinite `for {}` loop (ForStmt, no condition)
2. `continue` in `for i := 1; i <= n; i++` loop (ForCStmt)
3. `for x != 1` conditional loop (ForStmt with condition)
4. Complex expressions: `x * 3 + 1`, `x / 2`, `x % 2`

## What Must NOT Break
- Existing native tests: hello, fib, ifelse
- 21/21 BEAM runtime tests
- Label scoping (fn@@label) must work for new functions too

## How I'll Verify It Works
- [ ] Compile test_native_break.v with VBEAM_TARGET=arm64
- [ ] Run binary, verify: sum_until_break(10)=55, sum_odds(10)=25, collatz_steps(27)=111
- [ ] Existing native tests still pass
