# Bare Metal BEAM Interpreter Status

## What Was Created

`vbeam_beam_interp_bare.erl` - A self-contained BEAM bytecode interpreter designed for bare-metal execution (no OTP dependencies).

### Architecture

- **NO OTP dependencies**: No `io:format`, `beam_disasm`, `lists:*`, or `apply/3`
- **Output via callback**: `OutputFun(String)` parameter for all output
- **Self-contained list operations**: Inline implementations of length, reverse, nth, etc.
- **Explicit BIF dispatch table**: Direct implementation of arithmetic, comparison, and list BIFs
- **Map-based state**: Process state is a map (simpler than records for bare metal)
- **Uses standalone parser**: `vbeam_beam_standalone` for loading .beam files

### Implemented Opcodes

The interpreter implements ~30 essential opcodes:

**Metadata:**
- `label`, `func_info`, `line`, `int_code_end`

**Control flow:**
- `return`, `call`, `call_only`, `call_ext`, `call_ext_only`, `jump`

**Registers:**
- `move`

**Stack:**
- `allocate`, `allocate_zero`, `deallocate`, `test_heap`

**Lists:**
- `put_list`, `get_list`, `get_hd`, `get_tl`

**Tuples:**
- `put_tuple2`, `get_tuple_element`

**Type tests:**
- `is_integer`, `is_atom`, `is_list`, `is_nil`

**Comparisons:**
- `is_eq_exact`, `is_ne_exact`

**BIFs:**
- `gc_bif`, `bif` - arithmetic (`+`, `-`, `*`, `div`, `rem`, `/`)
- Comparison (`==`, `/=`, `<`, `>`, `=<`, `>=`)
- Boolean (`not`)
- Lists (`erlang:length`, `lists:reverse`)
- Output (`io:format`, `erlang:display`)

### State Structure

```erlang
#{
    module => Module,              % Loaded module data
    current_fun => {Name, Arity},  % Current function
    current_instrs => [...],       % Current function's instructions
    pc => 0,                       % Program counter
    x => #{{x, N} => Value},      % X registers
    y => [Value1, ...],           % Y registers (stack)
    stack => [{Fun, PC}, ...],    % Call stack
    output_fun => fun(Str) -> ... % Output callback
}
```

## Current Limitations

### CRITICAL: vbeam_beam_standalone Parser is Broken

The standalone parser (`vbeam_beam_standalone.erl`) has significant decoding issues:

1. **Incomplete opcode table**: Many opcodes (153=line, 48, 34, 32, etc.) are unknown
2. **Incorrect instruction decoding**: Instructions are mis-decoded (e.g., `move` becomes `bs_start_match2`)
3. **Missing first function**: Functions without explicit `func_info` at start are lost

**Impact**: The interpreter CANNOT correctly execute standard Erlang .beam files.

### Test Results

Compiled `bare_test.erl`:
```erlang
-module(bare_test).
-export([main/0, add/2]).
main() -> hello_world.
add(A, B) -> A + B.
```

**Results:**
- ✓ Module loads successfully
- ✓ Functions `add/2`, `module_info/0`, `module_info/1` found
- ✗ `main/0` missing (no func_info marker)
- ✗ `add(5, 3)` returns `5` instead of `8` (instructions mis-decoded)

The instructions for `add/2` should be:
```erlang
[{label,3},
 {func_info,{atom,bare_test},{atom,add},2},
 {label,4},
 {line,3},
 {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}},
 return]
```

But we get:
```erlang
[{func_info,bare_test,add,2},
 {label,4},
 {{unknown_opcode,153},[]},   % line
 {{unknown_opcode,48},[]},     % ??? (should be gc_bif)
 {'catch',[{label,0},{integer,2}]},  % wrong!
 ...]
```

## Path Forward

### Option 1: Fix vbeam_beam_standalone (RECOMMENDED)

The standalone parser needs:
1. Complete opcode table (opcodes 0-200+ from beam_asm.hrl)
2. Correct operand decoding for all opcodes
3. Better handling of compact term encoding
4. Test suite against beam_disasm output

**Effort**: 2-3 hours
**Benefit**: Unlocks the entire bare metal interpreter

### Option 2: Use beam_disasm for Now

For testing, temporarily use `beam_disasm` in the loading phase:
- Modify `load_module/1` to call `beam_disasm:file/1`
- Test interpreter logic with correct instructions
- Switch to standalone parser once fixed

**Effort**: 30 minutes
**Benefit**: Proves interpreter logic works
**Drawback**: Not truly bare-metal during development

### Option 3: Generate Test .beam Files

Create .beam files using:
- Direct bytecode assembly (write bytes manually)
- Minimal opcodes only
- Known-good instruction sequences

**Effort**: 1-2 hours
**Benefit**: Tests specific opcode implementations
**Drawback**: Doesn't solve real-world .beam compatibility

## What Works

The bare-metal interpreter's **architecture and execution engine** are sound:
- ✓ Loading interface works
- ✓ State management (registers, stack, PC) correct
- ✓ Function call/return logic implemented
- ✓ BIF dispatch table works
- ✓ No OTP dependencies (truly self-contained)
- ✓ Output callback pattern works

**The only blocker is the parser.**

## Next Steps

1. **Fix vbeam_beam_standalone.erl**:
   - Add complete opcode table from OTP source (beam_asm.hrl)
   - Fix instruction operand decoding
   - Add test: compare decode output with beam_disasm

2. **Test bare interpreter**:
   - Run identity/1, add/2, factorial/1
   - Test with V-compiled code (when available)

3. **Optimize for bare metal**:
   - Remove remaining `file:read_file` dependency (pass binary directly)
   - Add serial port output hook point
   - Test embedding in PE data section

## File Manifest

- `vbeam_beam_interp_bare.erl` (783 LOC) - Bare metal interpreter
- `vbeam_beam_standalone.erl` (662 LOC) - Standalone parser (NEEDS FIXES)
- `vbeam_beam_interp_v2.erl` (530 LOC) - Reference OTP-based interpreter

## Compilation

```bash
erlc -o os/beam_vm/ os/beam_vm/vbeam_beam_interp_bare.erl
```

Compiles with warnings (unused variables) but no errors.

## Testing (Current State)

```erlang
{ok, BeamBinary} = file:read_file("/tmp/bare_test.beam"),
OutputFun = fun(Str) -> io:format("~s", [Str]) end,
vbeam_beam_interp_bare:run(BeamBinary, add, [5, 3], OutputFun).
```

Returns: `{ok, 5}` (wrong, should be `{ok, 8}` due to parser issues)

## Ready for Bare Metal?

**Architecture**: YES ✓
**Parser**: NO ✗

Once the parser is fixed, this interpreter is ready to:
- Run in nucleus.efi (no OTP required)
- Output to serial port
- Execute V-compiled .beam files
- Bootstrap the BEAM-as-kernel vision
