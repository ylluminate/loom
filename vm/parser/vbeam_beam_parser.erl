%% BEAM File Parser
%% Parses .beam file binary format (IFF-like chunk structure)
%% Reference: http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html

-module(vbeam_beam_parser).
-export([parse_file/1, parse_beam/1]).

-include_lib("kernel/include/file.hrl").

%% SECURITY: Maximum decompressed LitT size (64MB)
-define(MAX_LITT_SIZE, 64 * 1024 * 1024).

%% Parse a .beam file from disk
parse_file(Path) ->
    %% SECURITY: Check file size first (100MB max) - Finding #11
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} when Size > 100_000_000 ->
            {error, {file_too_large, Size, max, 100_000_000}};
        {ok, _} ->
            case file:read_file(Path) of
                {ok, Binary} -> parse_beam(Binary);
                {error, Reason} -> {error, {file_read, Reason}}
            end;
        {error, Reason} ->
            {error, {file_info, Reason}}
    end.

%% Parse BEAM binary format
parse_beam(<<"FOR1", Size:32, "BEAM", Rest/binary>>) ->
    case Size - 4 of  % Size includes "BEAM" tag (4 bytes)
        ExpectedSize when ExpectedSize =:= byte_size(Rest) ->
            parse_chunks(Rest, #{});
        _ ->
            {error, size_mismatch}
    end;
parse_beam(_) ->
    {error, invalid_beam_format}.

%% Parse all chunks in the BEAM file
parse_chunks(<<>>, Acc) ->
    {ok, Acc};
parse_chunks(Binary, Acc) ->
    case parse_chunk(Binary) of
        {ok, ChunkName, ChunkData, Rest} ->
            parse_chunks(Rest, Acc#{ChunkName => ChunkData});
        {error, Reason} ->
            {error, Reason}
    end.

%% Parse a single chunk
parse_chunk(<<Name:4/binary, Size:32, Data:Size/binary, Rest/binary>>) ->
    %% Chunks are aligned to 4-byte boundaries
    Padding = case Size rem 4 of
        0 -> 0;
        N -> 4 - N
    end,
    case Rest of
        <<_:Padding/binary, AlignedRest/binary>> ->
            ChunkName = chunk_id_to_atom(Name),
            case parse_chunk_data(ChunkName, Data) of
                {error, Reason} ->
                    %% Codex R34 Finding #5: Propagate parse errors instead of crashing
                    {error, {chunk_parse_failed, ChunkName, Reason}};
                ParsedData ->
                    {ok, ChunkName, ParsedData, AlignedRest}
            end;
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

%% Parse chunk data based on chunk type
parse_chunk_data('Code', Data) ->
    %% Code chunk contains:
    %% - SubSize (16 bits) - should be 16
    %% - InstructionSet (32 bits) - BEAM version
    %% - OpcodeMax (32 bits) - highest opcode used
    %% - LabelCount (32 bits)
    %% - FunctionCount (32 bits)
    %% - Code (rest of binary)
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
            {error, invalid_code_chunk}
    end;

parse_chunk_data('Atom', Data) ->
    %% Atom chunk (old format): Count (32 bits) followed by atoms
    parse_atoms(Data);

parse_chunk_data('AtU8', Data) ->
    %% AtU8 chunk (UTF-8 atoms): Count (32 bits) followed by UTF-8 atoms
    parse_atoms(Data);

parse_chunk_data('StrT', Data) ->
    %% String table - just a binary of concatenated strings
    Data;

parse_chunk_data('ImpT', Data) ->
    %% Import table: Count (32 bits) followed by {Module, Function, Arity} tuples
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_imports(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('ExpT', Data) ->
    %% Export table: Count (32 bits) followed by {Function, Arity, Label} tuples
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_exports(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('FunT', Data) ->
    %% Lambda/fun table
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_funs(Rest, Count, []);
        _ ->
            Data
    end;

parse_chunk_data('LitT', Data) ->
    %% Literal table - compressed with zlib
    %% SECURITY: Use bounded streaming decompression to prevent zip bomb DoS
    case Data of
        <<UncompressedSize:32, Compressed/binary>> when UncompressedSize =< ?MAX_LITT_SIZE ->
            case decompress_bounded(Compressed, ?MAX_LITT_SIZE) of
                {ok, Uncompressed} ->
                    %% Verify decompressed size matches declared size (if non-zero)
                    case {UncompressedSize, byte_size(Uncompressed)} of
                        {0, _} ->
                            %% Size 0 means no size declared, trust decompressed data
                            parse_literals(Uncompressed);
                        {Size, Size} ->
                            parse_literals(Uncompressed);
                        {Declared, Actual} ->
                            {error, {litt_size_mismatch, declared, Declared, actual, Actual}}
                    end;
                {error, Reason} ->
                    {error, {litt_decompress_failed, Reason}}
            end;
        <<UncompressedSize:32, _/binary>> ->
            {error, {litt_too_large, UncompressedSize, max, ?MAX_LITT_SIZE}};
        _ ->
            Data
    end;

parse_chunk_data('LocT', Data) ->
    %% Local function table (optional, for debugging)
    case Data of
        <<Count:32, Rest/binary>> ->
            parse_exports(Rest, Count, []);  % Same format as ExpT
        _ ->
            Data
    end;

parse_chunk_data('Attr', Data) ->
    %% CODEX R38 FINDING #3 FIX: Match {'EXIT', _} explicitly before generic term
    %% Attributes - Erlang term (external term format)
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;  % Failed to decode - return raw data
        Term when not is_reference(Term) -> Term;
        _ -> Data
    end;

parse_chunk_data('CInf', Data) ->
    %% CODEX R38 FINDING #3 FIX: Match {'EXIT', _} explicitly before generic term
    %% Compilation info - Erlang term
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;  % Failed to decode - return raw data
        Term when not is_reference(Term) -> Term;
        _ -> Data
    end;

parse_chunk_data('Abst', Data) ->
    %% CODEX R38 FINDING #3 FIX: Match {'EXIT', _} explicitly before generic term
    %% Abstract code - Erlang term
    case catch binary_to_term(Data, [safe]) of
        {'EXIT', _} -> Data;  % Failed to decode - return raw data
        Term when not is_reference(Term) -> Term;
        _ -> Data
    end;

parse_chunk_data('Line', Data) ->
    %% Line number information
    Data;

parse_chunk_data(_Other, Data) ->
    %% Unknown chunk - keep as binary
    Data.

%% Parse atom table
parse_atoms(<<Count:32, Rest/binary>>) ->
    %% FINDING 8 FIX: Cap atom count to prevent memory exhaustion
    %% FINDING R41-2 FIX: Normalize signed/unsigned atom count interpretation
    %% Some compilers set high bit in count field - treat as unsigned but cap reasonably
    MaxAtoms = 100000,
    %% Check if count looks unreasonable (>100k), try alternative interpretations
    NormalizedCount = if
        Count > MaxAtoms andalso Count > 16#80000000 ->
            %% High bit set - may be signed, try masking/reinterpret
            %% If treating as signed gives reasonable value, use that
            SignedCount = Count - 16#100000000,
            if
                SignedCount > 0 andalso SignedCount =< MaxAtoms -> SignedCount;
                true -> Count  %% Keep original if signed doesn't help
            end;
        true ->
            Count
    end,
    case NormalizedCount of
        N when N > MaxAtoms ->
            {error, {atom_count_too_large, N, max, MaxAtoms}};
        _ ->
            parse_atom_list(Rest, NormalizedCount, [])
    end;
parse_atoms(_) ->
    {error, malformed_atom_chunk}.

parse_atom_list(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
parse_atom_list(<<Len:8, Atom:Len/binary, Rest/binary>>, Count, Acc) ->
    %% Keep atoms as binaries to avoid exhausting atom table
    parse_atom_list(Rest, Count - 1, [Atom | Acc]);
parse_atom_list(_, _, Acc) ->
    {lists:reverse(Acc), <<>>}.

%% Parse import table
parse_imports(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_imports(<<Module:32, Function:32, Arity:32, Rest/binary>>, Count, Acc) ->
    parse_imports(Rest, Count - 1, [{Module, Function, Arity} | Acc]);
parse_imports(_, _, Acc) ->
    lists:reverse(Acc).

%% Parse export table
parse_exports(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_exports(<<Function:32, Arity:32, Label:32, Rest/binary>>, Count, Acc) ->
    parse_exports(Rest, Count - 1, [{Function, Arity, Label} | Acc]);
parse_exports(_, _, Acc) ->
    lists:reverse(Acc).

%% Parse fun table
parse_funs(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_funs(<<Function:32, Arity:32, CodePos:32, Index:32,
             NumFree:32, OldUniq:32, Rest/binary>>, Count, Acc) ->
    Fun = #{function => Function,
            arity => Arity,
            code_pos => CodePos,
            index => Index,
            num_free => NumFree,
            old_uniq => OldUniq},
    parse_funs(Rest, Count - 1, [Fun | Acc]);
parse_funs(_, _, Acc) ->
    lists:reverse(Acc).

%% SECURITY: Bounded streaming decompression (prevents zip bomb DoS)
%% Decompresses data with hard output size cap, aborts if exceeded
decompress_bounded(Compressed, MaxSize) ->
    Z = zlib:open(),
    try
        zlib:inflateInit(Z),
        decompress_chunks(Z, Compressed, MaxSize, <<>>)
    catch
        _:Reason ->
            {error, Reason}
    after
        zlib:close(Z)
    end.

%% Inflate in chunks, abort if total output exceeds MaxSize
decompress_chunks(Z, <<>>, MaxSize, Acc) ->
    %% End of input, flush remaining
    %% SECURITY: Finding #12 - check size after flush
    case zlib:inflate(Z, <<>>) of
        [] ->
            {ok, iolist_to_binary(Acc)};
        Chunks ->
            Final = iolist_to_binary([Acc | Chunks]),
            case byte_size(Final) > MaxSize of
                true ->
                    {error, {output_exceeds_limit, MaxSize}};
                false ->
                    {ok, Final}
            end
    end;
decompress_chunks(Z, Data, MaxSize, Acc) ->
    %% Process in 8KB chunks
    ChunkSize = min(8192, byte_size(Data)),
    <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
    case zlib:inflate(Z, Chunk) of
        Decompressed ->
            %% SECURITY: Finding #13 - Use iolist accumulation (O(1) append) instead of binary concat (O(n²))
            NewAcc = [Acc | Decompressed],
            %% Check cumulative size via iolist_size
            case iolist_size(NewAcc) > MaxSize of
                true ->
                    {error, {output_exceeds_limit, MaxSize}};
                false ->
                    decompress_chunks(Z, Rest, MaxSize, NewAcc)
            end
    end.

%% Parse literal table
%% SECURITY FIX: Cap at 1,000,000 entries and verify Count doesn't exceed byte size
parse_literals(<<Count:32, Rest/binary>>) when Count =< 1_000_000 ->
    %% Verify Count doesn't exceed remaining bytes / minimum entry size (4 bytes)
    case byte_size(Rest) >= Count * 4 of
        true ->
            parse_literal_list(Rest, Count, []);
        false ->
            []  %% Invalid - not enough bytes for declared count
    end;
parse_literals(_) ->
    [].

parse_literal_list(_Rest, 0, Acc) ->
    lists:reverse(Acc);
parse_literal_list(<<Size:32, Literal:Size/binary, Rest/binary>>, Count, Acc) ->
    Term = try binary_to_term(Literal, [safe])
           catch
               error:badarg -> {error, unsafe_literal}
           end,
    parse_literal_list(Rest, Count - 1, [Term | Acc]);
parse_literal_list(_, _, Acc) ->
    lists:reverse(Acc).
