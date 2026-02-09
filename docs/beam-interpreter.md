## BEAM Bytecode Interpreter

A minimal BEAM bytecode interpreter for bootstrapping the bare-metal BEAM VM.

### Architecture

The interpreter consists of three main components:

1. **vbeam_beam_parser.erl** - Parses .beam file binary format (IFF-like chunk structure)
2. **vbeam_beam_interp.erl** - Interprets BEAM bytecode instructions
3. **vbeam_beam_run.escript** - Command-line runner for executing .beam files

### Design Strategy

Following the OS architecture principle: **"Don't implement all BEAM opcodes first. Compile your initial kernel services to BEAM bytecode and compute the opcode closure actually used. Implement that closure, boot, then expand."**

This means:
1. Start with a minimal opcode set
2. Compile V programs to .beam
3. Analyze which opcodes are actually used
4. Implement those opcodes
5. Iterate until the kernel boots

### Current Implementation Status

**Parser (vbeam_beam_parser.erl)**: ✅ Complete
- Parses IFF-like FOR1/BEAM chunk structure
- Extracts all standard chunks:
  - Code (bytecode instructions)
  - AtU8/Atom (atom table)
  - StrT (string table)
  - ImpT (import table - external functions)
  - ExpT (export table)
  - FunT (lambda/closure table)
  - LitT (literal table with zlib decompression)
  - Attr, CInf, Abst (metadata)

**Interpreter (vbeam_beam_interp.erl)**: ⚠️ Prototype Only

Current implementation is a **simplified prototype** for understanding the architecture. Real BEAM uses:

1. **Compact Encoding**: BEAM uses variable-length encoding for instructions and arguments
   - Tag-based encoding for different value types
   - Efficient representation of small integers
   - See: http://beam-wisdoms.clau.se/en/latest/indepth-beam-instructions.html

2. **Generic BEAM Instructions**: The .beam file contains "generic" instructions that are transformed to "specific" instructions at load time
   - Need to implement the transformation tables
   - Or use the BEAM disassembler (`beam_disasm:file/1`)

3. **Process State**: Full process state includes:
   - X registers (x0-x1023): temporary/argument registers
   - Y registers (y0-y1023): stack frame slots
   - Floating-point registers (separate)
   - Current module, function, arity
   - Program counter
   - Continuation pointer (CP)
   - Call stack
   - Heap and stack pointers
   - Catch/try stack

### Recommended Approach

Instead of building a full bytecode decoder from scratch, use Erlang's built-in disassembler:

```erlang
%% Get human-readable instruction list
{beam_file, Module, Exports, Attrs, CompileInfo, Code} =
    beam_disasm:file("path/to/file.beam").

%% Code is a list of functions:
[{function, Name, Arity, EntryLabel, Instructions}]
```

This gives you the decoded instructions in a much easier format to interpret.

### Essential Opcodes (Priority List)

Based on V-compiled BEAM programs, implement these opcodes first:

**Control Flow (highest priority)**:
- `label` - marks jump targets
- `func_info` - function metadata
- `return` - return from function
- `jump` - unconditional jump
- `call`, `call_only`, `call_last` - local function calls
- `call_ext`, `call_ext_only`, `call_ext_last` - external function calls
- `call_fun` - closure invocation

**Stack Management**:
- `allocate`, `allocate_zero` - allocate stack frame
- `deallocate` - deallocate stack frame
- `move` - move value between registers

**Data Construction**:
- `put_list` - construct cons cell [H|T]
- `put_tuple2` - construct tuple
- `put_map_assoc` - construct/update map
- `make_fun2`, `make_fun3` - create closure

**Data Destructuring**:
- `get_list` - destructure cons cell
- `get_tuple_element` - extract tuple element
- `get_hd`, `get_tl` - list head/tail
- `get_map_elements` - extract map values

**Pattern Matching**:
- `is_eq_exact`, `is_ne_exact` - equality tests
- `is_lt`, `is_ge`, `is_gt`, `is_le` - comparisons
- `is_atom`, `is_integer`, `is_float`, `is_tuple`, `is_list`, `is_nil`, `is_binary` - type tests
- `is_boolean`, `is_map` - more type tests
- `select_val` - switch/case dispatch
- `test_arity` - tuple arity test

**Binary Operations**:
- `bs_init2` - initialize binary builder
- `bs_put_integer`, `bs_put_binary`, `bs_put_string` - append to binary
- `bs_match_string`, `bs_get_integer`, `bs_get_binary` - pattern match binary

**Built-in Functions**:
- `gc_bif1`, `gc_bif2`, `gc_bif3` - BIF calls with GC
- `bif0`, `bif1`, `bif2` - BIF calls without GC

**Error Handling**:
- `badmatch`, `case_end`, `if_end` - error opcodes
- `try`, `try_end`, `try_case`, `try_case_end` - try/catch
- `catch`, `catch_end` - old-style catch

**Memory Management**:
- `test_heap` - ensure heap space available
- `trim` - trim stack frame

### Opcode Closure Analysis

To find which opcodes are actually used:

```bash
# Compile V program to BEAM
v -b beam -o /tmp/test.beam tests/runtime/hello.v

# Disassemble and extract opcodes
cd /tmp/test.beam
for f in *.beam; do
    erl -noshell -eval "{beam_file, _, _, _, _, Code} = beam_disasm:file(\"$f\"),
        Ops = lists:usort([element(1, I) || {function, _, _, _, Instructions} <- Code,
                                             I <- Instructions, is_tuple(I)]),
        io:format(\"~p: ~p~n\", [\"$f\", Ops]),
        halt(0)"
done
```

This will show you the exact set of opcodes you need to implement.

### Process Execution Model

```erlang
-record(proc, {
    module,         % Current module atom
    function,       % Current function atom
    arity,          % Current function arity
    code,           % Disassembled code (from beam_disasm)
    labels,         % Label -> Instruction mapping
    x = #{},        % X registers (arguments/temporaries)
    y = [],         % Y registers (stack frame)
    fr = #{},       % Float registers
    pc = 0,         % Program counter (instruction index)
    cp = undefined, % Continuation pointer (return address)
    stack = [],     % Call stack
    heap = []       % Heap (for now, just use Erlang terms)
}).
```

### BIF Dispatch

Since the interpreter runs inside Erlang, many BIFs can be directly delegated:

```erlang
execute_bif(erlang, '+', [A, B]) -> {ok, A + B};
execute_bif(erlang, 'display', [Term]) -> io:format("~p~n", [Term]), {ok, ok};
execute_bif(io, 'format', [Fmt, Args]) -> io:format(Fmt, Args), {ok, ok};
%% etc.
```

For the bare-metal port, these will need native implementations.

### Testing Strategy

1. **Phase 1**: Test with simple Erlang modules
   ```erlang
   -module(test).
   -export([add/2]).
   add(A, B) -> A + B.
   ```

2. **Phase 2**: Test with V-compiled hello world
   ```v
   fn main() { println('Hello') }
   ```

3. **Phase 3**: Test with V-compiled kernel services
   - Process spawning
   - Message passing
   - Basic I/O

4. **Phase 4**: Compute opcode closure
   - Collect all opcodes from kernel
   - Implement missing opcodes
   - Boot the kernel

### Usage

```bash
# Compile the interpreter
cd os/beam_vm
erlc vbeam_beam_parser.erl vbeam_beam_interp.erl

# Run a BEAM file
./vbeam_beam_run.escript /path/to/file.beam function_name [args...]

# Example
./vbeam_beam_run.escript /tmp/test_simple.beam add 5 3
```

### Next Steps

1. ✅ Parser complete
2. ⚠️ Rewrite interpreter to use `beam_disasm:file/1` for instruction decoding
3. ⏳ Implement essential opcodes from the priority list
4. ⏳ Test with simple Erlang modules
5. ⏳ Test with V-compiled hello world
6. ⏳ Analyze opcode closure for V kernel
7. ⏳ Implement missing opcodes
8. ⏳ Boot V kernel on the interpreter

### References

- [BEAM File Format](http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html)
- [BEAM Instructions](http://beam-wisdoms.clau.se/en/latest/indepth-beam-instructions.html)
- [The BEAM Book](https://blog.stenmans.org/theBeamBook/)
- [beam_disasm module](https://www.erlang.org/doc/apps/compiler/beam_disasm.html)
- [OTP BEAM Loader Source](https://github.com/erlang/otp/blob/master/erts/emulator/beam/beam_load.c)
