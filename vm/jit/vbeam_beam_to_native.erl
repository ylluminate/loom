%%% @doc BEAM-to-native x86_64 translator for V-on-BEAM.
%%% Reads .beam files and generates bare-metal x86_64 code.
%%% @end
-module(vbeam_beam_to_native).

-include_lib("kernel/include/file.hrl").

-export([translate_beam/1, translate_beam/2, translate_function/2]).
-export([serial_putchar_code/0, serial_puts_code/0]).

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Translate a .beam file to x86_64 machine code.
-spec translate_beam(file:filename()) ->
    {ok, #{code => binary(), data => binary(), entry => non_neg_integer()}} |
    {error, term()}.
translate_beam(BeamFile) ->
    translate_beam(BeamFile, #{}).

%% @doc Translate a .beam file with options.
-spec translate_beam(file:filename(), map()) ->
    {ok, #{code => binary(), data => binary(), entry => non_neg_integer()}} |
    {error, term()}.
translate_beam(BeamFile, _Options) ->
    %% SECURITY FIX (Finding #3): Check file size before reading (100MB max)
    case file:read_file_info(BeamFile) of
        {ok, FileInfo} ->
            Size = FileInfo#file_info.size,
            case Size > 100_000_000 of
                true ->
                    {error, {file_too_large, Size, max, 100_000_000}};
                false ->
                    case file:read_file(BeamFile) of
                        {ok, BeamBinary} ->
                            case vbeam_beam_standalone:parse_binary(BeamBinary) of
                                {ok, ParsedData} ->
                                    translate_chunks(ParsedData);
                                {error, Reason} ->
                                    {error, {beam_parse_error, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {file_read_error, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, {file_info_error, Reason}}
    end.

%% @doc Translate a single function's bytecode to x86_64.
-spec translate_function([term()], map()) -> {ok, binary()} | {error, term()}.
translate_function(Bytecodes, _Options) ->
    try
        Code = lists:foldl(fun translate_opcode/2, <<>>, Bytecodes),
        {ok, Code}
    catch
        error:Reason:Stack ->
            {error, {translation_error, Reason, Stack}}
    end.

%% ============================================================================
%% Serial Output Code Generation
%% ============================================================================

%% @doc Generate x86_64 code for serial putchar.
%% Input: AL = character to output
%% Clobbers: DX, AL
serial_putchar_code() ->
    iolist_to_binary([
        %% wait_tx_ready:
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,  % mov edx, 0x3FD (LSR)
        <<16#EC>>,                                % in al, dx
        <<16#A8, 16#20>>,                         % test al, 0x20 (TX empty?)
        <<16#74, 16#F6>>,                         % jz wait_tx_ready (-10 bytes)

        %% Write character (in BL, saved by caller)
        <<16#88, 16#D8>>,                         % mov al, bl
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,   % mov edx, 0x3F8 (THR)
        <<16#EE>>,                                % out dx, al
        <<16#C3>>                                 % ret
    ]).

%% @doc Generate x86_64 code for serial puts (length-bounded string).
%% Input: RSI = pointer to string
%%        RCX = length (number of bytes to output, max 4096)
%% Clobbers: RSI, RCX, RAX, RBX, RDX
%% SECURITY: Length-bounded to prevent reading past buffer boundaries
serial_puts_code() ->
    SerialPutcharSize = byte_size(serial_putchar_code()),

    %% DISPLACEMENT FIX (Round 25, Finding 2): Recalculated after CLD insertion
    %% Instruction layout (byte-by-byte):
    %%   +0: cld (1 byte)                    <<16#FC>>
    %%   +1: test rcx,rcx (3 bytes)          <<16#48, 16#85, 16#C9>>
    %%   +4: jz done (2 bytes)               <<16#74, disp8>>
    %%   +6: lodsb (1 byte)                  <<16#AC>>
    %%   +7: mov bl,al (2 bytes)             <<16#88, 16#C3>>
    %%   +9: dec rcx (3 bytes)               <<16#48, 16#FF, 16#C9>>
    %%   +12: call rel32 (5 bytes)           <<16#E8, disp32>>
    %%   +17: jmp loop (2 bytes)             <<16#EB, disp8>>
    %%   +19: ret (1 byte)                   <<16#C3>>
    %%
    %% Call target: serial_putchar is at -(SerialPutcharSize + 17)
    %%   (displacement from END of call instruction at +17)
    %% JZ target: ret at +19, from end of jz at +6 → +13 bytes
    %% JMP target: test at +1, from end of jmp at +19 → -18 bytes
    CallOffset = -(SerialPutcharSize + 17),

    iolist_to_binary([
        %% Clear direction flag before lodsb loop (CRITICAL: prevents backward reads)
        <<16#FC>>,                                % cld (clear direction flag)
        %% loop:
        <<16#48, 16#85, 16#C9>>,                  % test rcx, rcx (check counter)
        <<16#74, 16#0D>>,                         % jz done (+13 bytes to ret)
        <<16#AC>>,                                % lodsb (load byte from [RSI++] to AL)
        <<16#88, 16#C3>>,                         % mov bl, al (save char)
        <<16#48, 16#FF, 16#C9>>,                  % dec rcx (decrement counter)
        <<16#E8, CallOffset:32/little-signed>>,   % call serial_putchar
        <<16#EB, 16#EE>>,                         % jmp loop (-18 bytes)
        %% done:
        <<16#C3>>                                 % ret
    ]).

%% ============================================================================
%% Internal Translation
%% ============================================================================

%% SECURITY: Translation size limits
-define(MAX_FUNCTIONS, 10_000).
-define(MAX_INSTRUCTIONS_PER_FUNCTION, 100_000).
-define(MAX_OUTPUT_CODE_SIZE, 100_000_000).  % 100MB

%% Translate chunks from beam file (standalone parser format)
translate_chunks(ParsedData) ->
    try
        %% ParsedData is a map: #{atoms, exports, imports, code, code_info, literals, strings, funs}
        Atoms = maps:get(atoms, ParsedData, []),
        CodeBinary = maps:get(code, ParsedData, <<>>),

        case CodeBinary of
            <<>> ->
                {error, no_code_chunk};
            _ ->
                %% Decode instructions from bytecode using standalone parser
                %% CRITICAL FIX (Finding #5): Propagate decode errors instead of compiling truncated stream
                case vbeam_beam_standalone:decode_instructions(CodeBinary, Atoms) of
                    {error, DecodeError} ->
                        {error, {beam_decode_error, DecodeError}};
                    {ok, Instructions} ->
                        %% SECURITY FIX (Finding #3): Check total instruction count
                        InstrCount = length(Instructions),
                        case InstrCount > ?MAX_INSTRUCTIONS_PER_FUNCTION of
                            true ->
                                {error, {instruction_count_exceeded, InstrCount, max, ?MAX_INSTRUCTIONS_PER_FUNCTION}};
                            false ->
                                %% Split flat instruction list into per-function lists
                                Functions = extract_functions(Instructions),

                        %% SECURITY FIX (Finding #3): Check function count
                        FunctionCount = length(Functions),
                        case FunctionCount > ?MAX_FUNCTIONS of
                            true ->
                                {error, {function_count_exceeded, FunctionCount, max, ?MAX_FUNCTIONS}};
                            false ->
                                %% SECURITY FIX (Finding #3): Check instruction count per function
                                case check_function_sizes(Functions) of
                                    ok ->
                                        %% Build helper functions
                                        SerialPutchar = serial_putchar_code(),
                                        SerialPuts = serial_puts_code(),

                                        %% Translate each function
                                        TranslatedFunctions = [translate_function_internal(F) || F <- Functions],

                                        %% Combine code sections
                                        %% Layout: [serial_putchar][serial_puts][function1][function2]...
                                        CodeSections = [SerialPutchar, SerialPuts | TranslatedFunctions],
                                        Code = iolist_to_binary(CodeSections),

                                        %% SECURITY FIX (Finding #3): Check output code size
                                        CodeSize = byte_size(Code),
                                        case CodeSize > ?MAX_OUTPUT_CODE_SIZE of
                                            true ->
                                                {error, {output_code_size_exceeded, CodeSize, max, ?MAX_OUTPUT_CODE_SIZE}};
                                            false ->
                                                %% Entry point is first translated function (after helpers)
                                                EntryOffset = byte_size(SerialPutchar) + byte_size(SerialPuts),

                                                %% Data section (empty for now - strings embedded in code via RIP-relative)
                                                Data = <<>>,

                                                {ok, #{code => Code, data => Data, entry => EntryOffset}}
                                        end;
                                    {error, CheckError} ->
                                        {error, CheckError}
                                end
                        end
                end
                end  %% close {ok, Instructions} case from Finding #5 fix
        end
    catch
        error:CatchReason:Stack ->
            {error, {translation_failed, CatchReason, Stack}}
    end.

%% SECURITY FIX (Finding #3): Check that no function exceeds instruction limit
check_function_sizes([]) ->
    ok;
check_function_sizes([Function | Rest]) ->
    InstrCount = length(Function),
    case InstrCount > ?MAX_INSTRUCTIONS_PER_FUNCTION of
        true ->
            {error, {function_instruction_count_exceeded, InstrCount, max, ?MAX_INSTRUCTIONS_PER_FUNCTION}};
        false ->
            check_function_sizes(Rest)
    end.

%% Extract function definitions from flat instruction list
%% Split by func_info markers into per-function instruction lists
extract_functions(Instructions) ->
    split_functions(Instructions, [], []).

%% Split instruction list by func_info markers
split_functions([], CurrentFun, Acc) ->
    %% Save final function if exists
    NewAcc = case CurrentFun of
        [] -> Acc;
        _ -> [lists:reverse(CurrentFun) | Acc]
    end,
    %% Return accumulated functions (in original order)
    lists:reverse(NewAcc);

split_functions([{func_info, [_Mod, _Fun, _Arity]} | Rest], CurrentFun, Acc) ->
    %% New function starts - save previous if exists, start new
    NewAcc = case CurrentFun of
        [] -> Acc;  %% First function
        _ -> [lists:reverse(CurrentFun) | Acc]
    end,
    %% Start new function (don't include func_info in instructions)
    split_functions(Rest, [], NewAcc);

split_functions([Instr | Rest], CurrentFun, Acc) ->
    %% Accumulate instruction into current function
    split_functions(Rest, [Instr | CurrentFun], Acc).

%% Translate a single function's instruction list
translate_function_internal(Instructions) ->
    lists:foldl(fun translate_opcode/2, <<>>, Instructions).

%% ============================================================================
%% Opcode Translation
%% ============================================================================

%% BEAM register mapping to x86_64:
%% x0 -> RAX (accumulator)
%% x1 -> RBX (general)
%% x2 -> RCX (general, MS x64 arg1)
%% x3 -> RDX (general, MS x64 arg2)
%% y(N) -> [RSP + N*8] (stack slot)

%% Translate a single BEAM opcode to x86_64
%% First normalize instruction shape from decoded format {op, [Operands...]} to expected format
translate_opcode(Instruction, Acc) ->
    translate_opcode_normalized(normalize_instruction(Instruction), Acc).

%% Normalize decoded instruction format to expected format
normalize_instruction({label, [{integer, N}]}) ->
    {label, N};
normalize_instruction({line, _} = I) ->
    I;
normalize_instruction({func_info, [Mod, Fun, {integer, Arity}]}) ->
    {func_info, Mod, Fun, Arity};
normalize_instruction({allocate, [{integer, N}, {integer, M}]}) ->
    {allocate, N, M};
normalize_instruction({deallocate, [{integer, N}]}) ->
    {deallocate, N};
normalize_instruction({move, [Src, Dst]}) ->
    {move, Src, Dst};
normalize_instruction({call_ext, [{integer, Arity}, ExtFunc]}) ->
    {call_ext, Arity, ExtFunc};
normalize_instruction({call_ext_last, [{integer, Arity}, ExtFunc, {integer, Dealloc}]}) ->
    {call_ext_last, Arity, ExtFunc, Dealloc};
normalize_instruction({return, []}) ->
    return;
normalize_instruction({test_heap, [{integer, Need}, {integer, Live}]}) ->
    {test_heap, Need, Live};
normalize_instruction({put_string, [{integer, Len}, String, Dst]}) ->
    {put_string, Len, String, Dst};
normalize_instruction({bs_put_string, [{integer, Len}, String]}) ->
    {bs_put_string, Len, String};
normalize_instruction(Other) ->
    %% Already normalized or unknown format - pass through
    Other.

translate_opcode_normalized({label, _N}, Acc) ->
    %% Labels are positional - we'd need multi-pass for full support
    %% For now, just mark position (no code emitted)
    Acc;

translate_opcode_normalized({line, _}, Acc) ->
    %% Debug info - skip
    Acc;

translate_opcode_normalized({func_info, _Mod, _Fun, _Arity}, Acc) ->
    %% Function info for error handling - skip for now
    Acc;

translate_opcode_normalized({allocate, StackNeeded, _Live}, Acc) ->
    %% Allocate stack frame: sub rsp, N*8
    StackBytes = StackNeeded * 8,
    %% FIX 2: Handle large frames (>31 words = >248 bytes)
    Code = if
        StackBytes =< 127 ->
            %% 8-bit immediate encoding
            <<16#48, 16#83, 16#EC, StackBytes:8>>;  % sub rsp, imm8
        true ->
            %% 32-bit immediate encoding for large frames
            <<16#48, 16#81, 16#EC, StackBytes:32/little>>  % sub rsp, imm32
    end,
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({deallocate, N}, Acc) ->
    %% Deallocate stack frame: add rsp, N*8
    StackBytes = N * 8,
    %% FIX 2: Handle large frames (>31 words = >248 bytes)
    Code = if
        StackBytes =< 127 ->
            %% 8-bit immediate encoding
            <<16#48, 16#83, 16#C4, StackBytes:8>>;  % add rsp, imm8
        true ->
            %% 32-bit immediate encoding for large frames
            <<16#48, 16#81, 16#C4, StackBytes:32/little>>  % add rsp, imm32
    end,
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({move, Src, Dst}, Acc) ->
    %% Move between registers/stack slots
    Code = translate_move(Src, Dst),
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({call_ext, _Arity, {extfunc, Mod, Fun, _A}}, Acc) ->
    %% External function call - handle io:format, erlang:display as serial output
    Code = translate_external_call(Mod, Fun),
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({call_ext_last, _Arity, {extfunc, Mod, Fun, _A}, Dealloc}, Acc) ->
    %% Tail call - deallocate then call
    Code1 = translate_opcode_normalized({deallocate, Dealloc}, <<>>),
    Code2 = translate_external_call(Mod, Fun),
    <<Acc/binary, Code1/binary, Code2/binary>>;

translate_opcode_normalized(return, Acc) ->
    %% Return from function
    Code = <<16#C3>>,  % ret
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({test_heap, _Need, _Live}, Acc) ->
    %% Heap allocation test - skip for bare metal (no GC)
    Acc;

translate_opcode_normalized({put_string, Len, String, Dst}, Acc) ->
    %% Load string pointer into register
    %% For bare metal, we embed string and load address
    %% Simplified: load immediate (would need proper RIP-relative addressing)
    Code = translate_put_string(Len, String, Dst),
    <<Acc/binary, Code/binary>>;

translate_opcode_normalized({bs_put_string, _Len, _String}, Acc) ->
    %% Binary string operation - skip for now
    Acc;

translate_opcode_normalized(UnknownOp, _Acc) ->
    %% SECURITY FIX: Fail closed on unknown opcodes instead of silently producing NOP
    %% This prevents guest code from using undocumented/future opcodes to bypass translation
    error({unknown_opcode, UnknownOp}).

%% ============================================================================
%% Move Translation
%% ============================================================================

translate_move({x, 0}, {x, 1}) ->
    <<16#48, 16#89, 16#C3>>;  % mov rbx, rax

translate_move({x, 1}, {x, 0}) ->
    <<16#48, 16#89, 16#D8>>;  % mov rax, rbx

translate_move({x, 0}, {x, 2}) ->
    <<16#48, 16#89, 16#C1>>;  % mov rcx, rax

translate_move({x, 2}, {x, 0}) ->
    <<16#48, 16#89, 16#C8>>;  % mov rax, rcx

translate_move({x, 0}, {y, N}) ->
    %% mov [rsp+N*8], rax
    Offset = N * 8,
    if
        Offset < 128 ->
            <<16#48, 16#89, 16#44, 16#24, Offset:8>>;  % mov [rsp+disp8], rax
        true ->
            <<16#48, 16#89, 16#84, 16#24, Offset:32/little>>  % mov [rsp+disp32], rax
    end;

translate_move({y, N}, {x, 0}) ->
    %% mov rax, [rsp+N*8]
    Offset = N * 8,
    if
        Offset < 128 ->
            <<16#48, 16#8B, 16#44, 16#24, Offset:8>>;  % mov rax, [rsp+disp8]
        true ->
            <<16#48, 16#8B, 16#84, 16#24, Offset:32/little>>  % mov rax, [rsp+disp32]
    end;

translate_move({integer, Val}, {x, 0}) ->
    %% mov rax, immediate
    if
        Val >= -16#80000000, Val =< 16#7FFFFFFF ->
            %% 32-bit sign-extended immediate
            <<16#48, 16#C7, 16#C0, Val:32/little-signed>>;  % mov rax, imm32
        true ->
            %% 64-bit immediate (movabs)
            <<16#48, 16#B8, Val:64/little-signed>>  % movabs rax, imm64
    end;

translate_move({atom, _Atom}, {x, 0}) ->
    %% For atoms, we'd need an atom table - for now, load 0
    <<16#48, 16#31, 16#C0>>;  % xor rax, rax

%% SECURITY FIX (Finding #4): Return error instead of silently emitting NOP
translate_move(Src, Dst) ->
    error({unsupported_move, Src, Dst}).

%% ============================================================================
%% External Call Translation
%% ============================================================================

translate_external_call(io, format) ->
    translate_serial_output();

translate_external_call(erlang, display) ->
    translate_serial_output();

%% SECURITY FIX (Finding #5): Return error instead of silently emitting NOP
translate_external_call(Mod, Fun) ->
    error({unknown_external, Mod, Fun}).

%% Generate inline serial output code
%% Assumes RSI points to string (loaded by caller from put_string)
translate_serial_output() ->
    %% Inline string output loop - this is expanded INLINE in the function body,
    %% so we must NOT end with 'ret' (that would return from the caller prematurely)
    %% FIX 3: Removed 'ret' instruction - this is inline code, not a helper function
    iolist_to_binary([
        %% Inline string output loop
        %% RSI = string pointer (assumed already loaded)
        <<16#FC>>,                                % cld (clear direction flag - CRITICAL)
        <<16#AC>>,                                % lodsb
        <<16#84, 16#C0>>,                         % test al, al
        <<16#74, 16#16>>,                         % jz done (+22 bytes)
        <<16#88, 16#C3>>,                         % mov bl, al
        %% Wait for TX ready
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,   % mov edx, 0x3FD
        <<16#EC>>,                                % in al, dx
        <<16#A8, 16#20>>,                         % test al, 0x20
        <<16#74, 16#FB>>,                         % jz wait (-5)
        <<16#88, 16#D8>>,                         % mov al, bl
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,   % mov edx, 0x3F8
        <<16#EE>>,                                % out dx, al
        <<16#EB, 16#E5>>                          % jmp loop (-27)
        %% done: (fall through to next instruction in caller)
    ]).

%% ============================================================================
%% String Embedding
%% ============================================================================

%% SECURITY FIX (Finding #8): Emit error value instead of null pointer
%% BUG FIX #4: Must clear RSI to prevent lodsb from reading stale pointer
translate_put_string(_Len, String, {x, 0}) ->
    %% For bare metal, we need RIP-relative addressing
    %% String data isn't available yet - emit code that loads error sentinel
    %% instead of dereferencing null
    _ = String,  % Suppress warning
    %% Load error sentinel value (-1) and clear RSI
    <<16#48, 16#C7, 16#C0, 16#FF, 16#FF, 16#FF, 16#FF,  % mov rax, -1
      16#48, 16#31, 16#F6>>;  % xor rsi, rsi (clear to prevent stale reads)

translate_put_string(_Len, _String, _Dst) ->
    %% Load error sentinel and clear RSI
    <<16#48, 16#C7, 16#C0, 16#FF, 16#FF, 16#FF, 16#FF,  % mov rax, -1
      16#48, 16#31, 16#F6>>.  % xor rsi, rsi
