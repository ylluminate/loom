# vbeam_beam_standalone - Self-Contained BEAM Parser

A standalone BEAM file parser for V-on-BEAM that requires **NO OTP dependencies**.

## Features

- ✅ No dependencies on `beam_disasm`, `beam_lib`, or `compile`
- ✅ Parses all essential BEAM chunks (AtU8/Atom, Code, ExpT, ImpT, LitT, StrT, FunT)
- ✅ Decodes BEAM instructions to symbolic form
- ✅ Handles compact term encoding used in atom tables
- ✅ Decompresses literal table (zlib)
- ✅ ~62% instruction coverage (132+ opcodes)
- ✅ Self-parsing capable

## API

### parse_file/1

```erlang
parse_file(Filename) -> {ok, Result} | {error, Reason}
```

Reads and parses a .beam file from disk.

### parse_binary/1

```erlang
parse_binary(Binary) -> {ok, Result} | {error, Reason}
```

Parses a BEAM file from binary data.

**Result format:**
```erlang
#{
    atoms => [atom()],           %% Atom table
    exports => [{FunIdx, Arity, Label}],
    imports => [{ModIdx, FunIdx, Arity}],
    code => binary(),            %% Raw bytecode
    code_info => #{              %% Code chunk metadata
        instruction_set => integer(),
        opcode_max => integer(),
        label_count => integer(),
        function_count => integer()
    },
    literals => [term()],        %% Literal table (decompressed)
    strings => binary(),         %% String table
    funs => [FunInfo]           %% Lambda/closure table
}
```

### decode_instructions/2

```erlang
decode_instructions(CodeBinary, AtomTable) -> [Instruction]
```

Decodes bytecode to symbolic instructions.

**Returns:**
```erlang
[{OpcodeName, [Operand]}]

where Operand ::= {x, N}           % X register
                | {y, N}           % Y register (stack)
                | {atom, Atom}     % Atom reference
                | {integer, Int}   % Integer literal
                | {literal, Idx}   % Literal table index
                | {label, N}       % Jump label
                | {list, [Operand]} % List of operands
                | {alloc, [{Type, Count}]} % Allocation list
```

## File Structure

- **Size:** 662 lines, 20KB source, 8.3KB compiled
- **Functions:** 24 unique functions
- **Opcode coverage:** 132 opcodes (62% of typical BEAM files)

## Usage Example

```erlang
%% Parse a BEAM file
{ok, Result} = vbeam_beam_standalone:parse_file("mymodule.beam"),

%% Extract atoms and code
Atoms = maps:get(atoms, Result),
Code = maps:get(code, Result),

%% Decode instructions
Instructions = vbeam_beam_standalone:decode_instructions(Code, Atoms),

%% Pretty-print first 10 instructions
lists:foreach(fun(Instr) ->
    io:format("~p~n", [Instr])
end, lists:sublist(Instructions, 10)).
```

## Limitations

- ~38% of opcodes show as `{unknown_opcode, N}` (less common operations)
- Extended compact term encoding only supports values up to 2 bytes
- No support for optional chunks (Dbgi, Line) - returns as binary
- Allocation list decoding is simplified

## Testing

```bash
cd /Users/u/tank/ventures/v_projects/vbeam__v_on_beam/os/beam_vm
erlc test_standalone_parser.erl
erl -noshell -s test_standalone_parser run -s init stop
```

## Why This Exists

The existing `vbeam_beam_interp_v2.erl` depends on `beam_disasm:file/1` which requires full OTP. For a bare-metal BEAM VM booting via UEFI, we need a parser that works with only core Erlang modules (`binary`, `lists`, `zlib`, `file`, `erlang`).

This parser provides that foundation.
