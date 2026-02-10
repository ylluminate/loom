%% Standalone BEAM File Parser
%% NO OTP dependencies (no beam_disasm, beam_lib, compile)
%% Parses .beam file binary format and decodes instructions
%%
%% BEAM file format (IFF-like):
%% - Header: <<"FOR1", Size:32, "BEAM">>
%% - Chunks: <<ChunkId:4/binary, ChunkSize:32, Data:ChunkSize/binary, Padding>>
%%
%% Essential chunks:
%% - AtU8/Atom: atom table
%% - Code: bytecode
%% - ExpT: export table
%% - ImpT: import table
%% - StrT: string table
%% - LitT: literal table (zlib compressed)
%% - FunT: lambda/closure table

-module(vbeam_beam_standalone).
-export([parse_file/1, parse_binary/1, decode_instructions/2]).

%% SECURITY: Maximum decompressed LitT size (64MB)
-define(MAX_LITT_SIZE, 64 * 1024 * 1024).

%% ============================================================================
%% Public API
%% ============================================================================

%% Parse a .beam file from disk
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> parse_binary(Binary);
        {error, Reason} -> {error, {file_read, Reason}}
    end.

%% Parse BEAM binary format
parse_binary(<<"FOR1", Size:32, "BEAM", Rest/binary>>) ->
    ExpectedSize = Size - 4,  % Size includes "BEAM" tag (4 bytes)
    case byte_size(Rest) of
        ExpectedSize ->
            case parse_chunks(Rest, #{}) of
                {ok, Chunks} ->
                    build_result(Chunks);
                Error ->
                    Error
            end;
        _ ->
            {error, size_mismatch}
    end;
parse_binary(_) ->
    {error, invalid_beam_format}.

%% Decode instructions from Code chunk binary
%% Args: CodeBinary (from Code chunk's 'code' field), AtomTable (list of atoms)
%% Returns: list of {Opcode, [Operand1, Operand2, ...]}
decode_instructions(CodeBinary, AtomTable) ->
    decode_instr_loop(CodeBinary, AtomTable, []).

%% ============================================================================
%% Chunk Parsing
%% ============================================================================

parse_chunks(<<>>, Acc) ->
    {ok, Acc};
parse_chunks(Binary, Acc) ->
    case parse_chunk(Binary) of
        {ok, ChunkName, ChunkData, Rest} ->
            parse_chunks(Rest, Acc#{ChunkName => ChunkData});
        {error, Reason} ->
            {error, Reason}
    end.

parse_chunk(<<Name:4/binary, Size:32, Data:Size/binary, Rest/binary>>) ->
    %% Chunks aligned to 4-byte boundaries
    Padding = case Size rem 4 of
        0 -> 0;
        N -> 4 - N
    end,
    case Rest of
        <<_:Padding/binary, AlignedRest/binary>> ->
            ChunkName = chunk_id_to_atom(Name),
            ParsedData = parse_chunk_data(ChunkName, Data),
            {ok, ChunkName, ParsedData, AlignedRest};
        _ ->
            {error, truncated_chunk}
    end;
parse_chunk(_) ->
    {error, invalid_chunk}.

%% Map chunk IDs to atoms using whitelist (prevents atom table exhaustion)
%% Standard BEAM chunk IDs from the BEAM file format specification
chunk_id_to_atom(<<"Atom">>) -> 'Atom';
chunk_id_to_atom(<<"AtU8">>) -> 'AtU8';
chunk_id_to_atom(<<"Code">>) -> 'Code';
chunk_id_to_atom(<<"StrT">>) -> 'StrT';
chunk_id_to_atom(<<"ImpT">>) -> 'ImpT';
chunk_id_to_atom(<<"ExpT">>) -> 'ExpT';
chunk_id_to_atom(<<"FunT">>) -> 'FunT';
chunk_id_to_atom(<<"LitT">>) -> 'LitT';
chunk_id_to_atom(<<"LocT">>) -> 'LocT';
chunk_id_to_atom(<<"Attr">>) -> 'Attr';
chunk_id_to_atom(<<"CInf">>) -> 'CInf';
chunk_id_to_atom(<<"Dbgi">>) -> 'Dbgi';
chunk_id_to_atom(<<"Docs">>) -> 'Docs';
chunk_id_to_atom(<<"ExDp">>) -> 'ExDp';
chunk_id_to_atom(<<"Line">>) -> 'Line';
chunk_id_to_atom(<<"Meta">>) -> 'Meta';
chunk_id_to_atom(<<"Type">>) -> 'Type';
chunk_id_to_atom(<<"Abst">>) -> 'Abst';  % Abstract code chunk
chunk_id_to_atom(Other) -> {unknown_chunk, Other}.  % Keep as binary for unknown chunks

%% ============================================================================
%% Chunk Data Parsers
%% ============================================================================

parse_chunk_data('Code', Data) ->
    case Data of
        <<SubSize:32, InstructionSet:32, OpcodeMax:32,
          LabelCount:32, FunctionCount:32, Code/binary>> ->
            #{sub_size => SubSize,
              instruction_set => InstructionSet,
              opcode_max => OpcodeMax,
              label_count => LabelCount,
              function_count => FunctionCount,
              code => Code};
        _ ->
            Data
    end;

parse_chunk_data('Atom', Data) ->
    parse_atoms(Data);

parse_chunk_data('AtU8', Data) ->
    parse_atoms(Data);

parse_chunk_data('StrT', Data) ->
    Data;

parse_chunk_data('ImpT', Data) ->
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_imports(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('ExpT', Data) ->
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_exports(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('FunT', Data) ->
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_funs(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('LitT', Data) ->
    %% SECURITY: Limit decompressed size to 64MB to prevent memory exhaustion
    case Data of
        <<UncompressedSize:32, Compressed/binary>> when UncompressedSize =< ?MAX_LITT_SIZE ->
            %% Try zlib decompression first (standard OTP BEAM files)
            case catch zlib:uncompress(Compressed) of
                Uncompressed when is_binary(Uncompressed) ->
                    %% Verify decompressed size matches declared size
                    case byte_size(Uncompressed) of
                        UncompressedSize ->
                            parse_literals(Uncompressed);
                        _ActualSize when UncompressedSize =:= 0 ->
                            %% Size 0 means not compressed or size not declared
                            parse_literals(Uncompressed);
                        ActualSize ->
                            {error, {litt_size_mismatch, declared, UncompressedSize, actual, ActualSize}}
                    end;
                _ ->
                    %% Decompression failed — data may be uncompressed
                    %% (V compiler and some tools write uncompressed LitT)
                    case UncompressedSize of
                        0 ->
                            %% Size 0 means not compressed, parse directly
                            parse_literals(Compressed);
                        _ ->
                            %% Try parsing as-is (fallback)
                            case catch parse_literals(Compressed) of
                                L when is_list(L) -> L;
                                _ ->
                                    #{uncompressed_size => UncompressedSize,
                                      compressed => Compressed}
                            end
                    end
            end;
        <<UncompressedSize:32, _/binary>> ->
            {error, {litt_too_large, UncompressedSize, max, ?MAX_LITT_SIZE}};
        _ ->
            Data
    end;

parse_chunk_data('LocT', Data) ->
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_exports(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('Attr', Data) ->
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;
        Term -> Term
    end;

parse_chunk_data('CInf', Data) ->
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;
        Term -> Term
    end;

parse_chunk_data('Abst', Data) ->
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;
        Term -> Term
    end;

parse_chunk_data('Line', Data) ->
    Data;

parse_chunk_data(_Other, Data) ->
    Data.

%% ============================================================================
%% Table Parsers
%% ============================================================================

parse_atoms(Binary) ->
    %% Try different formats for atom count
    %% Modern BEAM files use compact encoding, but let's try signed int first
    case Binary of
        <<Count:32/signed, Rest/binary>> when Count < 0 ->
            %% Negative count means use absolute value
            parse_atom_list(Rest, abs(Count), []);
        <<Count:32, Rest/binary>> when Count > 0, Count < 10000 ->
            %% Positive count
            parse_atom_list(Rest, Count, []);
        _ ->
            []
    end.

parse_atom_list(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_atom_list(Binary, Count, Acc) ->
    %% Decode atom length using compact term encoding
    case decode_compact_term_int(Binary) of
        {Len, Rest} when Len > 0 ->
            case Rest of
                <<Atom:Len/binary, Rest2/binary>> ->
                    %% Keep atoms as binaries to avoid exhausting atom table
                    parse_atom_list(Rest2, Count - 1, [Atom | Acc]);
                _ ->
                    lists:reverse(Acc)
            end;
        _ ->
            lists:reverse(Acc)
    end.

parse_imports(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_imports(Rest, Count, Acc) when Count > 0 ->
    %% Bound count by remaining bytes (12 bytes per entry)
    MaxCount = byte_size(Rest) div 12,
    if
        Count > MaxCount ->
            {error, {impossibly_large_import_count, Count, MaxCount}};
        true ->
            parse_imports_loop(Rest, Count, Acc)
    end;
parse_imports(_, _, Acc) ->
    lists:reverse(Acc).

parse_imports_loop(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_imports_loop(<<Module:32, Function:32, Arity:32, Rest/binary>>, Count, Acc) ->
    parse_imports_loop(Rest, Count - 1, [{Module, Function, Arity} | Acc]);
parse_imports_loop(_, _, Acc) ->
    {error, {truncated_imports, lists:reverse(Acc)}}.

parse_exports(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_exports(Rest, Count, Acc) when Count > 0 ->
    %% Bound count by remaining bytes (12 bytes per entry)
    MaxCount = byte_size(Rest) div 12,
    if
        Count > MaxCount ->
            {error, {impossibly_large_export_count, Count, MaxCount}};
        true ->
            parse_exports_loop(Rest, Count, Acc)
    end;
parse_exports(_, _, Acc) ->
    lists:reverse(Acc).

parse_exports_loop(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_exports_loop(<<Function:32, Arity:32, Label:32, Rest/binary>>, Count, Acc) ->
    parse_exports_loop(Rest, Count - 1, [{Function, Arity, Label} | Acc]);
parse_exports_loop(_, _, Acc) ->
    {error, {truncated_exports, lists:reverse(Acc)}}.

parse_funs(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_funs(Rest, Count, Acc) when Count > 0 ->
    %% Bound count by remaining bytes (24 bytes per entry)
    MaxCount = byte_size(Rest) div 24,
    if
        Count > MaxCount ->
            {error, {impossibly_large_fun_count, Count, MaxCount}};
        true ->
            parse_funs_loop(Rest, Count, Acc)
    end;
parse_funs(_, _, Acc) ->
    lists:reverse(Acc).

parse_funs_loop(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_funs_loop(<<Function:32, Arity:32, CodePos:32, Index:32,
             NumFree:32, OldUniq:32, Rest/binary>>, Count, Acc) ->
    Fun = #{function => Function,
            arity => Arity,
            code_pos => CodePos,
            index => Index,
            num_free => NumFree,
            old_uniq => OldUniq},
    parse_funs_loop(Rest, Count - 1, [Fun | Acc]);
parse_funs_loop(_, _, Acc) ->
    {error, {truncated_funs, lists:reverse(Acc)}}.

parse_literals(<<Count:32, Rest/binary>>) ->
    parse_literal_list(Rest, Count, []).

parse_literal_list(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_literal_list(<<Size:32, Literal:Size/binary, Rest/binary>>, Count, Acc) ->
    Term = try binary_to_term(Literal, [safe])
           catch
               error:Reason -> {error, {bad_literal, Reason}}
           end,
    parse_literal_list(Rest, Count - 1, [Term | Acc]);
parse_literal_list(_, _, Acc) ->
    lists:reverse(Acc).

%% ============================================================================
%% Result Builder
%% ============================================================================

build_result(Chunks) ->
    Atoms = case maps:get('AtU8', Chunks, undefined) of
        undefined -> maps:get('Atom', Chunks, []);
        A -> A
    end,

    Exports = maps:get('ExpT', Chunks, []),
    Imports = maps:get('ImpT', Chunks, []),
    Literals = maps:get('LitT', Chunks, []),
    Strings = maps:get('StrT', Chunks, <<>>),
    Funs = maps:get('FunT', Chunks, []),

    CodeChunk = maps:get('Code', Chunks, #{}),
    CodeBinary = maps:get(code, CodeChunk, <<>>),

    {ok, #{
        atoms => Atoms,
        exports => Exports,
        imports => Imports,
        code => CodeBinary,
        code_info => maps:remove(code, CodeChunk),
        literals => Literals,
        strings => Strings,
        funs => Funs
    }}.

%% ============================================================================
%% Compact Term Decoder (used in atom table and instruction operands)
%% ============================================================================

%% Decode compact term format used in BEAM files
%% Tag encoding (bits 2:0):
%%   0 = Literal (small integer)
%%   1 = Integer
%%   2 = Atom
%%   3 = X register
%%   4 = Y register
%%   5 = Label
%%   6 = Character
%%   7 = Extended list
%%
%% For tags 0-1, if bits 4:3 = 0, value in bits 7:4 (0-15)
%% If bits 4:3 = 1 (bit 3 set), next byte contains value
%% If bits 4:3 = 2 (bit 4 set), error
%% If bits 4:3 = 3 (both set), extended multi-byte
decode_compact_term_int(<<Byte:8, Rest/binary>>) ->
    Tag = Byte band 2#111,
    Val = Byte bsr 4,

    %% Check if extended (bit 3 set means next byte is value)
    IsExtended = (Byte band 2#1000) =/= 0,

    case {Tag, IsExtended} of
        {0, false} ->  %% Literal, value in upper 4 bits
            {Val, Rest};
        {0, true} ->   %% Literal, value in next byte
            case Rest of
                <<V:8, Rest2/binary>> ->
                    {V, Rest2};
                _ ->
                    {0, <<>>}
            end;
        {1, false} ->  %% Integer, value in upper 4 bits
            {Val, Rest};
        {1, true} ->   %% Integer, value in next byte
            case Rest of
                <<V:8, Rest2/binary>> ->
                    {V, Rest2};
                _ ->
                    {0, <<>>}
            end;
        {_, _} ->      %% Other tags
            {Val, Rest}
    end;
decode_compact_term_int(_) ->
    {0, <<>>}.

%% ============================================================================
%% Instruction Decoder
%% ============================================================================

%% Decode instruction stream
decode_instr_loop(<<>>, _Atoms, Acc) ->
    lists:reverse(Acc);
decode_instr_loop(Binary, Atoms, Acc) ->
    case decode_instruction(Binary, Atoms) of
        {Instr, Rest} ->
            decode_instr_loop(Rest, Atoms, [Instr | Acc]);
        error ->
            %% If we can't decode, return what we have
            lists:reverse(Acc)
    end.

%% Decode a single instruction
%% Returns: {{Opcode, [Operands]}, RestBinary} | error
decode_instruction(<<Opcode:8, Rest/binary>>, Atoms) ->
    OpcodeName = opcode_to_name(Opcode),
    Arity = opcode_arity(Opcode),
    case decode_operands(Rest, Arity, Atoms, []) of
        {Operands, Rest2} ->
            {{OpcodeName, Operands}, Rest2};
        error ->
            error
    end;
decode_instruction(_, _) ->
    error.

%% Decode N operands
decode_operands(Rest, 0, _Atoms, Acc) ->
    {lists:reverse(Acc), Rest};
decode_operands(Binary, N, Atoms, Acc) ->
    case decode_operand(Binary, Atoms) of
        {Operand, Rest} ->
            decode_operands(Rest, N - 1, Atoms, [Operand | Acc]);
        error ->
            error
    end.

%% Decode a single operand using BEAM compact term encoding
%% Tag in bits 2:0: 0=u, 1=i, 2=atom, 3=x-reg, 4=y-reg, 5=label, 6=char, 7=extended
%% Value encoding: bit3=0 → 4-bit val in bits 7:4
%%                 bit3=1,bit4=0 → 11-bit val (3 bits from byte + 8 from next)
%%                 bit3=1,bit4=1 → multi-byte (length from bits 7:5)
decode_operand(<<Byte:8, Rest/binary>>, Atoms) ->
    Tag = Byte band 16#07,
    case Tag of
        16#07 ->  %% Extended (z tag)
            decode_extended(Byte bsr 4, Rest, Atoms);
        _ ->
            %% Decode the integer value, then interpret by tag
            {Val, Rest2} = decode_compact_value(Byte, Rest),
            interpret_tag(Tag, Val, Rest2, Atoms)
    end;
decode_operand(_, _) ->
    error.

%% Decode the value portion of a compact term (handles multi-byte)
%% Matches OTP's decode_int logic from beam_disasm.erl
decode_compact_value(Byte, Rest) ->
    case (Byte band 16#08) of
        0 ->
            %% Small: value in bits 7:4 (0-15)
            {Byte bsr 4, Rest};
        _ ->
            case (Byte band 16#10) of
                0 ->
                    %% Medium: 11-bit value = 3 bits from byte (bits 7:5) + 8 from next
                    %% Encoding: bits 7:5 hold high 3 bits, next byte holds low 8 bits
                    %% Formula: ((Byte >> 5) & 7) | (NextByte << 3)
                    case Rest of
                        <<Next:8, Rest2/binary>> ->
                            HighBits = (Byte bsr 5) band 7,  %% Extract bits 7:5
                            {HighBits bor (Next bsl 3), Rest2};
                        _ ->
                            {0, Rest}
                    end;
                _ ->
                    %% Large: variable-length integer
                    decode_large_value(Byte, Rest)
            end
    end.

%% Decode large (multi-byte) integer values
decode_large_value(Byte, Rest) ->
    LenCode = Byte bsr 5,
    case LenCode of
        7 ->
            %% LenCode=7: Recursive length encoding (rare, used for very large integers)
            %% Next byte(s) encode the length itself as a compact term
            case decode_compact_value_recursive(Rest) of
                {Len, Rest2} when Len > 0 ->
                    case Rest2 of
                        <<Bytes:Len/binary, Rest3/binary>> ->
                            N = build_int_from_bytes(Bytes, 0),
                            <<FirstByte:8, _/binary>> = Bytes,
                            Val = case FirstByte > 127 of
                                true -> N - (1 bsl (Len * 8));
                                false -> N
                            end,
                            {Val, Rest3};
                        _ ->
                            {0, Rest}
                    end;
                _ ->
                    {0, Rest}
            end;
        _ ->
            %% LenCode 0-6: Fixed length (2-8 bytes)
            Len = LenCode + 2,
            case Rest of
                <<Bytes:Len/binary, Rest2/binary>> ->
                    N = build_int_from_bytes(Bytes, 0),
                    %% Check for negative (signed integer)
                    <<FirstByte:8, _/binary>> = Bytes,
                    Val = case FirstByte > 127 of
                        true -> N - (1 bsl (Len * 8));
                        false -> N
                    end,
                    {Val, Rest2};
                _ ->
                    {0, Rest}
            end
    end.

%% Recursive compact value decoder for LenCode=7 case
decode_compact_value_recursive(Binary) ->
    decode_compact_value(Binary, Binary).

%% Build integer from big-endian bytes
build_int_from_bytes(<<B:8, Rest/binary>>, Acc) ->
    build_int_from_bytes(Rest, (Acc bsl 8) bor B);
build_int_from_bytes(<<>>, Acc) ->
    Acc.

%% Interpret decoded value based on tag type
interpret_tag(16#00, Val, Rest, _Atoms) ->  %% u (unsigned/literal)
    {{integer, Val}, Rest};
interpret_tag(16#01, Val, Rest, _Atoms) ->  %% i (integer)
    {{integer, Val}, Rest};
interpret_tag(16#02, 0, Rest, _Atoms) ->    %% atom index 0 = nil
    {{atom, nil}, Rest};
interpret_tag(16#02, Val, Rest, Atoms) ->   %% atom index
    Atom = case Val > 0 andalso Val =< length(Atoms) of
        true -> lists:nth(Val, Atoms);
        false -> Val
    end,
    {{atom, Atom}, Rest};
interpret_tag(16#03, Val, Rest, _Atoms) ->  %% x register
    {{x, Val}, Rest};
interpret_tag(16#04, Val, Rest, _Atoms) ->  %% y register
    {{y, Val}, Rest};
interpret_tag(16#05, Val, Rest, _Atoms) ->  %% label
    {{label, Val}, Rest};
interpret_tag(16#06, Val, Rest, _Atoms) ->  %% character
    {{char, Val}, Rest}.

%% Decode extended operand types (tag 7 / z)
%% Sub-type from bits 7:4 — OTP numbering from beam_disasm.erl:
%%   0 = float, 1 = list, 2 = float register, 3 = alloc list, 4 = literal
decode_extended(0, Rest, _Atoms) ->
    %% Float - next 8 bytes are IEEE 754 double
    case Rest of
        <<Float:64/float, Rest2/binary>> ->
            {{float, Float}, Rest2};
        _ ->
            error
    end;
decode_extended(1, Rest, Atoms) ->
    %% List - count (as compact term) followed by count elements
    case decode_operand(Rest, Atoms) of
        {{integer, Count}, Rest2} when Count >= 0, Count =< 1000 ->
            case decode_operands(Rest2, Count, Atoms, []) of
                {Items, Rest3} ->
                    {{list, Items}, Rest3};
                error ->
                    error
            end;
        {{integer, _Count}, _Rest2} ->
            %% Count out of bounds (>1000)
            error;
        _ ->
            error
    end;
decode_extended(2, Rest, Atoms) ->
    %% Float register - register number as compact term
    case decode_operand(Rest, Atoms) of
        {{integer, Fr}, Rest2} ->
            {{fr, Fr}, Rest2};
        _ ->
            error
    end;
decode_extended(3, Rest, Atoms) ->
    %% Allocation list - count as compact term, then type/val pairs
    case decode_operand(Rest, Atoms) of
        {{integer, Count}, Rest2} ->
            decode_alloc_list(Rest2, Count, Atoms, []);
        _ ->
            error
    end;
decode_extended(4, Rest, Atoms) ->
    %% Literal - index into literal table as compact term
    case decode_operand(Rest, Atoms) of
        {{integer, Idx}, Rest2} ->
            {{literal, Idx}, Rest2};
        _ ->
            error
    end;
decode_extended(_, Rest, _Atoms) ->
    %% Unknown extended type
    {{unknown, extended}, Rest}.

%% Decode allocation list entries (type/val pairs as compact terms)
decode_alloc_list(Rest, 0, _Atoms, Acc) ->
    {{alloc, lists:reverse(Acc)}, Rest};
decode_alloc_list(Rest, N, Atoms, Acc) ->
    case decode_operand(Rest, Atoms) of
        {{integer, Type}, Rest2} ->
            case decode_operand(Rest2, Atoms) of
                {{integer, Val}, Rest3} ->
                    TypeName = case Type of
                        0 -> words;
                        1 -> floats;
                        2 -> funs;
                        _ -> {unknown, Type}
                    end,
                    decode_alloc_list(Rest3, N - 1, Atoms, [{TypeName, Val} | Acc]);
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% ============================================================================
%% Opcode Table
%% ============================================================================

%% Map opcode byte to name — OTP 28 numbering (from beam_opcodes:opname/1)
opcode_to_name(1) -> label;
opcode_to_name(2) -> func_info;
opcode_to_name(3) -> int_code_end;
opcode_to_name(4) -> call;
opcode_to_name(5) -> call_last;
opcode_to_name(6) -> call_only;
opcode_to_name(7) -> call_ext;
opcode_to_name(8) -> call_ext_last;
opcode_to_name(9) -> bif0;
opcode_to_name(10) -> bif1;
opcode_to_name(11) -> bif2;
opcode_to_name(12) -> allocate;
opcode_to_name(13) -> allocate_heap;
opcode_to_name(14) -> allocate_zero;
opcode_to_name(15) -> allocate_heap_zero;
opcode_to_name(16) -> test_heap;
opcode_to_name(17) -> init;
opcode_to_name(18) -> deallocate;
opcode_to_name(19) -> return;
opcode_to_name(20) -> send;
opcode_to_name(21) -> remove_message;
opcode_to_name(22) -> timeout;
opcode_to_name(23) -> loop_rec;
opcode_to_name(24) -> loop_rec_end;
opcode_to_name(25) -> wait;
opcode_to_name(26) -> wait_timeout;
opcode_to_name(27) -> m_plus;
opcode_to_name(28) -> m_minus;
opcode_to_name(29) -> m_times;
opcode_to_name(30) -> m_div;
opcode_to_name(31) -> int_div;
opcode_to_name(32) -> int_rem;
opcode_to_name(33) -> int_band;
opcode_to_name(34) -> int_bor;
opcode_to_name(35) -> int_bxor;
opcode_to_name(36) -> int_bsl;
opcode_to_name(37) -> int_bsr;
opcode_to_name(38) -> int_bnot;
opcode_to_name(39) -> is_lt;
opcode_to_name(40) -> is_ge;
opcode_to_name(41) -> is_eq;
opcode_to_name(42) -> is_ne;
opcode_to_name(43) -> is_eq_exact;
opcode_to_name(44) -> is_ne_exact;
opcode_to_name(45) -> is_integer;
opcode_to_name(46) -> is_float;
opcode_to_name(47) -> is_number;
opcode_to_name(48) -> is_atom;
opcode_to_name(49) -> is_pid;
opcode_to_name(50) -> is_reference;
opcode_to_name(51) -> is_port;
opcode_to_name(52) -> is_nil;
opcode_to_name(53) -> is_binary;
opcode_to_name(54) -> is_constant;
opcode_to_name(55) -> is_list;
opcode_to_name(56) -> is_nonempty_list;
opcode_to_name(57) -> is_tuple;
opcode_to_name(58) -> test_arity;
opcode_to_name(59) -> select_val;
opcode_to_name(60) -> select_tuple_arity;
opcode_to_name(61) -> jump;
opcode_to_name(62) -> 'catch';
opcode_to_name(63) -> catch_end;
opcode_to_name(64) -> move;
opcode_to_name(65) -> get_list;
opcode_to_name(66) -> get_tuple_element;
opcode_to_name(67) -> set_tuple_element;
opcode_to_name(68) -> put_string;
opcode_to_name(69) -> put_list;
opcode_to_name(70) -> put_tuple;
opcode_to_name(71) -> put;
opcode_to_name(72) -> badmatch;
opcode_to_name(73) -> if_end;
opcode_to_name(74) -> case_end;
opcode_to_name(75) -> call_fun;
opcode_to_name(76) -> make_fun;
opcode_to_name(77) -> is_function;
opcode_to_name(78) -> call_ext_only;
opcode_to_name(79) -> bs_start_match;
opcode_to_name(80) -> bs_get_integer;
opcode_to_name(81) -> bs_get_float;
opcode_to_name(82) -> bs_get_binary;
opcode_to_name(83) -> bs_skip_bits;
opcode_to_name(84) -> bs_test_tail;
opcode_to_name(85) -> bs_save;
opcode_to_name(86) -> bs_restore;
opcode_to_name(87) -> bs_init;
opcode_to_name(88) -> bs_final;
opcode_to_name(89) -> bs_put_integer;
opcode_to_name(90) -> bs_put_binary;
opcode_to_name(91) -> bs_put_float;
opcode_to_name(92) -> bs_put_string;
opcode_to_name(93) -> bs_need_buf;
opcode_to_name(94) -> fclearerror;
opcode_to_name(95) -> fcheckerror;
opcode_to_name(96) -> fmove;
opcode_to_name(97) -> fconv;
opcode_to_name(98) -> fadd;
opcode_to_name(99) -> fsub;
opcode_to_name(100) -> fmul;
opcode_to_name(101) -> fdiv;
opcode_to_name(102) -> fnegate;
opcode_to_name(103) -> make_fun2;
opcode_to_name(104) -> 'try';
opcode_to_name(105) -> try_end;
opcode_to_name(106) -> try_case;
opcode_to_name(107) -> try_case_end;
opcode_to_name(108) -> raise;
opcode_to_name(109) -> bs_init2;
opcode_to_name(110) -> bs_bits_to_bytes;
opcode_to_name(111) -> bs_add;
opcode_to_name(112) -> apply;
opcode_to_name(113) -> apply_last;
opcode_to_name(114) -> is_boolean;
opcode_to_name(115) -> is_function2;
opcode_to_name(116) -> bs_start_match2;
opcode_to_name(117) -> bs_get_integer2;
opcode_to_name(118) -> bs_get_float2;
opcode_to_name(119) -> bs_get_binary2;
opcode_to_name(120) -> bs_skip_bits2;
opcode_to_name(121) -> bs_test_tail2;
opcode_to_name(122) -> bs_save2;
opcode_to_name(123) -> bs_restore2;
opcode_to_name(124) -> gc_bif1;
opcode_to_name(125) -> gc_bif2;
opcode_to_name(126) -> bs_final2;
opcode_to_name(127) -> bs_bits_to_bytes2;
opcode_to_name(128) -> put_literal;
opcode_to_name(129) -> is_bitstr;
opcode_to_name(130) -> bs_context_to_binary;
opcode_to_name(131) -> bs_test_unit;
opcode_to_name(132) -> bs_match_string;
opcode_to_name(133) -> bs_init_writable;
opcode_to_name(134) -> bs_append;
opcode_to_name(135) -> bs_private_append;
opcode_to_name(136) -> trim;
opcode_to_name(137) -> bs_init_bits;
opcode_to_name(138) -> bs_get_utf8;
opcode_to_name(139) -> bs_skip_utf8;
opcode_to_name(140) -> bs_get_utf16;
opcode_to_name(141) -> bs_skip_utf16;
opcode_to_name(142) -> bs_get_utf32;
opcode_to_name(143) -> bs_skip_utf32;
opcode_to_name(144) -> bs_utf8_size;
opcode_to_name(145) -> bs_put_utf8;
opcode_to_name(146) -> bs_utf16_size;
opcode_to_name(147) -> bs_put_utf16;
opcode_to_name(148) -> bs_put_utf32;
opcode_to_name(149) -> on_load;
opcode_to_name(150) -> recv_mark;
opcode_to_name(151) -> recv_set;
opcode_to_name(152) -> gc_bif3;
opcode_to_name(153) -> line;
opcode_to_name(154) -> put_map_assoc;
opcode_to_name(155) -> put_map_exact;
opcode_to_name(156) -> is_map;
opcode_to_name(157) -> has_map_fields;
opcode_to_name(158) -> get_map_elements;
opcode_to_name(159) -> is_tagged_tuple;
opcode_to_name(160) -> build_stacktrace;
opcode_to_name(161) -> raw_raise;
opcode_to_name(162) -> get_hd;
opcode_to_name(163) -> get_tl;
opcode_to_name(164) -> put_tuple2;
opcode_to_name(165) -> bs_get_tail;
opcode_to_name(166) -> bs_start_match3;
opcode_to_name(167) -> bs_get_position;
opcode_to_name(168) -> bs_set_position;
opcode_to_name(169) -> swap;
opcode_to_name(170) -> bs_start_match4;
opcode_to_name(171) -> make_fun3;
opcode_to_name(172) -> init_yregs;
opcode_to_name(173) -> recv_marker_bind;
opcode_to_name(174) -> recv_marker_clear;
opcode_to_name(175) -> recv_marker_reserve;
opcode_to_name(176) -> recv_marker_use;
opcode_to_name(177) -> bs_create_bin;
opcode_to_name(178) -> call_fun2;
opcode_to_name(179) -> nif_start;
opcode_to_name(180) -> badrecord;
opcode_to_name(N) -> {unknown_opcode, N}.

%% Return arity (number of operands) for each opcode — OTP 28 numbering
opcode_arity(1) -> 1;   % label
opcode_arity(2) -> 3;   % func_info
opcode_arity(3) -> 0;   % int_code_end
opcode_arity(4) -> 2;   % call
opcode_arity(5) -> 3;   % call_last
opcode_arity(6) -> 2;   % call_only
opcode_arity(7) -> 2;   % call_ext
opcode_arity(8) -> 3;   % call_ext_last
opcode_arity(9) -> 2;   % bif0
opcode_arity(10) -> 4;  % bif1
opcode_arity(11) -> 5;  % bif2
opcode_arity(12) -> 2;  % allocate
opcode_arity(13) -> 3;  % allocate_heap
opcode_arity(14) -> 2;  % allocate_zero
opcode_arity(15) -> 3;  % allocate_heap_zero
opcode_arity(16) -> 2;  % test_heap
opcode_arity(17) -> 1;  % init
opcode_arity(18) -> 1;  % deallocate
opcode_arity(19) -> 0;  % return
opcode_arity(20) -> 0;  % send
opcode_arity(21) -> 0;  % remove_message
opcode_arity(22) -> 0;  % timeout
opcode_arity(23) -> 2;  % loop_rec
opcode_arity(24) -> 1;  % loop_rec_end
opcode_arity(25) -> 1;  % wait
opcode_arity(26) -> 2;  % wait_timeout
opcode_arity(27) -> 4;  % m_plus
opcode_arity(28) -> 4;  % m_minus
opcode_arity(29) -> 4;  % m_times
opcode_arity(30) -> 4;  % m_div
opcode_arity(31) -> 4;  % int_div
opcode_arity(32) -> 4;  % int_rem
opcode_arity(33) -> 4;  % int_band
opcode_arity(34) -> 4;  % int_bor
opcode_arity(35) -> 4;  % int_bxor
opcode_arity(36) -> 4;  % int_bsl
opcode_arity(37) -> 4;  % int_bsr
opcode_arity(38) -> 3;  % int_bnot
opcode_arity(39) -> 3;  % is_lt
opcode_arity(40) -> 3;  % is_ge
opcode_arity(41) -> 3;  % is_eq
opcode_arity(42) -> 3;  % is_ne
opcode_arity(43) -> 3;  % is_eq_exact
opcode_arity(44) -> 3;  % is_ne_exact
opcode_arity(45) -> 2;  % is_integer
opcode_arity(46) -> 2;  % is_float
opcode_arity(47) -> 2;  % is_number
opcode_arity(48) -> 2;  % is_atom
opcode_arity(49) -> 2;  % is_pid
opcode_arity(50) -> 2;  % is_reference
opcode_arity(51) -> 2;  % is_port
opcode_arity(52) -> 2;  % is_nil
opcode_arity(53) -> 2;  % is_binary
opcode_arity(54) -> 2;  % is_constant
opcode_arity(55) -> 2;  % is_list
opcode_arity(56) -> 2;  % is_nonempty_list
opcode_arity(57) -> 2;  % is_tuple
opcode_arity(58) -> 3;  % test_arity
opcode_arity(59) -> 3;  % select_val
opcode_arity(60) -> 3;  % select_tuple_arity
opcode_arity(61) -> 1;  % jump
opcode_arity(62) -> 2;  % catch
opcode_arity(63) -> 1;  % catch_end
opcode_arity(64) -> 2;  % move
opcode_arity(65) -> 3;  % get_list
opcode_arity(66) -> 3;  % get_tuple_element
opcode_arity(67) -> 3;  % set_tuple_element
opcode_arity(68) -> 3;  % put_string
opcode_arity(69) -> 3;  % put_list
opcode_arity(70) -> 2;  % put_tuple
opcode_arity(71) -> 1;  % put
opcode_arity(72) -> 1;  % badmatch
opcode_arity(73) -> 0;  % if_end
opcode_arity(74) -> 1;  % case_end
opcode_arity(75) -> 1;  % call_fun
opcode_arity(76) -> 3;  % make_fun
opcode_arity(77) -> 2;  % is_function
opcode_arity(78) -> 2;  % call_ext_only
opcode_arity(79) -> 2;  % bs_start_match
opcode_arity(80) -> 5;  % bs_get_integer
opcode_arity(81) -> 5;  % bs_get_float
opcode_arity(82) -> 5;  % bs_get_binary
opcode_arity(83) -> 4;  % bs_skip_bits
opcode_arity(84) -> 2;  % bs_test_tail
opcode_arity(85) -> 1;  % bs_save
opcode_arity(86) -> 1;  % bs_restore
opcode_arity(87) -> 2;  % bs_init
opcode_arity(88) -> 2;  % bs_final
opcode_arity(89) -> 5;  % bs_put_integer
opcode_arity(90) -> 5;  % bs_put_binary
opcode_arity(91) -> 5;  % bs_put_float
opcode_arity(92) -> 2;  % bs_put_string
opcode_arity(93) -> 1;  % bs_need_buf
opcode_arity(94) -> 0;  % fclearerror
opcode_arity(95) -> 1;  % fcheckerror
opcode_arity(96) -> 2;  % fmove
opcode_arity(97) -> 2;  % fconv
opcode_arity(98) -> 4;  % fadd
opcode_arity(99) -> 4;  % fsub
opcode_arity(100) -> 4; % fmul
opcode_arity(101) -> 4; % fdiv
opcode_arity(102) -> 3; % fnegate
opcode_arity(103) -> 1; % make_fun2
opcode_arity(104) -> 2; % try
opcode_arity(105) -> 1; % try_end
opcode_arity(106) -> 1; % try_case
opcode_arity(107) -> 1; % try_case_end
opcode_arity(108) -> 2; % raise
opcode_arity(109) -> 6; % bs_init2
opcode_arity(110) -> 3; % bs_bits_to_bytes
opcode_arity(111) -> 5; % bs_add
opcode_arity(112) -> 1; % apply
opcode_arity(113) -> 2; % apply_last
opcode_arity(114) -> 2; % is_boolean
opcode_arity(115) -> 3; % is_function2
opcode_arity(116) -> 5; % bs_start_match2
opcode_arity(117) -> 7; % bs_get_integer2
opcode_arity(118) -> 7; % bs_get_float2
opcode_arity(119) -> 7; % bs_get_binary2
opcode_arity(120) -> 5; % bs_skip_bits2
opcode_arity(121) -> 3; % bs_test_tail2
opcode_arity(122) -> 2; % bs_save2
opcode_arity(123) -> 2; % bs_restore2
opcode_arity(124) -> 5; % gc_bif1
opcode_arity(125) -> 6; % gc_bif2
opcode_arity(126) -> 2; % bs_final2
opcode_arity(127) -> 2; % bs_bits_to_bytes2
opcode_arity(128) -> 2; % put_literal
opcode_arity(129) -> 2; % is_bitstr
opcode_arity(130) -> 1; % bs_context_to_binary
opcode_arity(131) -> 3; % bs_test_unit
opcode_arity(132) -> 4; % bs_match_string
opcode_arity(133) -> 0; % bs_init_writable
opcode_arity(134) -> 8; % bs_append
opcode_arity(135) -> 6; % bs_private_append
opcode_arity(136) -> 2; % trim
opcode_arity(137) -> 6; % bs_init_bits
opcode_arity(138) -> 5; % bs_get_utf8
opcode_arity(139) -> 4; % bs_skip_utf8
opcode_arity(140) -> 5; % bs_get_utf16
opcode_arity(141) -> 4; % bs_skip_utf16
opcode_arity(142) -> 5; % bs_get_utf32
opcode_arity(143) -> 4; % bs_skip_utf32
opcode_arity(144) -> 3; % bs_utf8_size
opcode_arity(145) -> 3; % bs_put_utf8
opcode_arity(146) -> 3; % bs_utf16_size
opcode_arity(147) -> 3; % bs_put_utf16
opcode_arity(148) -> 3; % bs_put_utf32
opcode_arity(149) -> 0; % on_load
opcode_arity(150) -> 1; % recv_mark
opcode_arity(151) -> 1; % recv_set
opcode_arity(152) -> 7; % gc_bif3
opcode_arity(153) -> 1; % line
opcode_arity(154) -> 5; % put_map_assoc
opcode_arity(155) -> 5; % put_map_exact
opcode_arity(156) -> 2; % is_map
opcode_arity(157) -> 3; % has_map_fields
opcode_arity(158) -> 3; % get_map_elements
opcode_arity(159) -> 4; % is_tagged_tuple
opcode_arity(160) -> 0; % build_stacktrace
opcode_arity(161) -> 0; % raw_raise
opcode_arity(162) -> 2; % get_hd
opcode_arity(163) -> 2; % get_tl
opcode_arity(164) -> 2; % put_tuple2
opcode_arity(165) -> 3; % bs_get_tail
opcode_arity(166) -> 4; % bs_start_match3
opcode_arity(167) -> 3; % bs_get_position
opcode_arity(168) -> 2; % bs_set_position
opcode_arity(169) -> 2; % swap
opcode_arity(170) -> 4; % bs_start_match4
opcode_arity(171) -> 3; % make_fun3
opcode_arity(172) -> 1; % init_yregs
opcode_arity(173) -> 2; % recv_marker_bind
opcode_arity(174) -> 1; % recv_marker_clear
opcode_arity(175) -> 1; % recv_marker_reserve
opcode_arity(176) -> 1; % recv_marker_use
opcode_arity(177) -> 6; % bs_create_bin
opcode_arity(178) -> 3; % call_fun2
opcode_arity(179) -> 0; % nif_start
opcode_arity(180) -> 1; % badrecord
opcode_arity(_) -> 0.   % unknown - assume no operands
