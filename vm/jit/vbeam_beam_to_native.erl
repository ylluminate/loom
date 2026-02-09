%%% @doc BEAM-to-native x86_64 translator for V-on-BEAM.
%%% Reads .beam files and generates bare-metal x86_64 code.
%%% @end
-module(vbeam_beam_to_native).

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
    case file:read_file(BeamFile) of
        {ok, BeamBinary} ->
            case beam_lib:chunks(BeamBinary, [code, atoms]) of
                {ok, {_Module, Chunks}} ->
                    translate_chunks(Chunks);
                {error, beam_lib, Reason} ->
                    {error, {beam_parse_error, Reason}}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
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

%% @doc Generate x86_64 code for serial puts (null-terminated string).
%% Input: RSI = pointer to null-terminated string
%% Clobbers: RSI, RAX, RBX, RDX
serial_puts_code() ->
    SerialPutcharSize = byte_size(serial_putchar_code()),

    %% Calculate relative call offset to serial_putchar
    %% serial_puts comes right after serial_putchar in the layout
    %% At the call instruction (offset 7), we need to call back to serial_putchar
    %% which is at offset -(SerialPutcharSize + 7)
    CallOffset = -(SerialPutcharSize + 7),

    iolist_to_binary([
        %% loop:
        <<16#AC>>,                                % lodsb (load byte from [RSI++] to AL)
        <<16#84, 16#C0>>,                         % test al, al
        <<16#74, 16#07>>,                         % jz done (+7 bytes to ret)
        <<16#88, 16#C3>>,                         % mov bl, al (save char)
        <<16#E8, CallOffset:32/little-signed>>,   % call serial_putchar
        <<16#EB, 16#F3>>,                         % jmp loop (-13 bytes)
        %% done:
        <<16#C3>>                                 % ret
    ]).

%% ============================================================================
%% Internal Translation
%% ============================================================================

%% Translate chunks from beam file
translate_chunks(Chunks) ->
    case proplists:get_value(code, Chunks) of
        undefined ->
            {error, no_code_chunk};
        CodeChunk ->
            %% Extract functions from code chunk
            %% CodeChunk format: {Module, Exports, Attributes, CompileInfo, Code}
            Functions = extract_functions(CodeChunk),

            %% Build helper functions
            SerialPutchar = serial_putchar_code(),
            SerialPuts = serial_puts_code(),

            %% Translate each function
            TranslatedFunctions = [translate_function_internal(F) || F <- Functions],

            %% Combine code sections
            %% Layout: [serial_putchar][serial_puts][function1][function2]...
            CodeSections = [SerialPutchar, SerialPuts | TranslatedFunctions],
            Code = iolist_to_binary(CodeSections),

            %% Entry point is first translated function (after helpers)
            EntryOffset = byte_size(SerialPutchar) + byte_size(SerialPuts),

            %% Data section (empty for now - strings embedded in code via RIP-relative)
            Data = <<>>,

            {ok, #{code => Code, data => Data, entry => EntryOffset}}
    end.

%% Extract function definitions from code chunk
extract_functions({_Module, _Exports, _Attrs, _CompileInfo, Code}) ->
    %% Code is a list of function definitions
    %% Each function: {function, Name, Arity, Entry, Instructions}
    [Instrs || {function, _Name, _Arity, _Entry, Instrs} <- Code].

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
translate_opcode({label, _N}, Acc) ->
    %% Labels are positional - we'd need multi-pass for full support
    %% For now, just mark position (no code emitted)
    Acc;

translate_opcode({line, _}, Acc) ->
    %% Debug info - skip
    Acc;

translate_opcode({func_info, _Mod, _Fun, _Arity}, Acc) ->
    %% Function info for error handling - skip for now
    Acc;

translate_opcode({allocate, StackNeeded, _Live}, Acc) ->
    %% Allocate stack frame: sub rsp, N*8
    StackBytes = StackNeeded * 8,
    Code = <<16#48, 16#83, 16#EC, StackBytes:8>>,  % sub rsp, imm8
    <<Acc/binary, Code/binary>>;

translate_opcode({deallocate, N}, Acc) ->
    %% Deallocate stack frame: add rsp, N*8
    StackBytes = N * 8,
    Code = <<16#48, 16#83, 16#C4, StackBytes:8>>,  % add rsp, imm8
    <<Acc/binary, Code/binary>>;

translate_opcode({move, Src, Dst}, Acc) ->
    %% Move between registers/stack slots
    Code = translate_move(Src, Dst),
    <<Acc/binary, Code/binary>>;

translate_opcode({call_ext, _Arity, {extfunc, Mod, Fun, _A}}, Acc) ->
    %% External function call - handle io:format, erlang:display as serial output
    Code = translate_external_call(Mod, Fun),
    <<Acc/binary, Code/binary>>;

translate_opcode({call_ext_last, _Arity, {extfunc, Mod, Fun, _A}, Dealloc}, Acc) ->
    %% Tail call - deallocate then call
    Code1 = translate_opcode({deallocate, Dealloc}, <<>>),
    Code2 = translate_external_call(Mod, Fun),
    <<Acc/binary, Code1/binary, Code2/binary>>;

translate_opcode(return, Acc) ->
    %% Return from function
    Code = <<16#C3>>,  % ret
    <<Acc/binary, Code/binary>>;

translate_opcode({test_heap, _Need, _Live}, Acc) ->
    %% Heap allocation test - skip for bare metal (no GC)
    Acc;

translate_opcode({put_string, Len, String, Dst}, Acc) ->
    %% Load string pointer into register
    %% For bare metal, we embed string and load address
    %% Simplified: load immediate (would need proper RIP-relative addressing)
    Code = translate_put_string(Len, String, Dst),
    <<Acc/binary, Code/binary>>;

translate_opcode({bs_put_string, _Len, _String}, Acc) ->
    %% Binary string operation - skip for now
    Acc;

translate_opcode(_UnknownOp, Acc) ->
    %% Unknown opcode - emit nop for now (should warn/error in production)
    <<Acc/binary, 16#90>>.  % nop

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
            <<16#48, 16#B8, Val:64/little>>  % movabs rax, imm64
    end;

translate_move({atom, _Atom}, {x, 0}) ->
    %% For atoms, we'd need an atom table - for now, load 0
    <<16#48, 16#31, 16#C0>>;  % xor rax, rax

translate_move(_Src, _Dst) ->
    %% Unhandled move - emit nop
    <<16#90>>.

%% ============================================================================
%% External Call Translation
%% ============================================================================

translate_external_call(io, format) ->
    translate_serial_output();

translate_external_call(erlang, display) ->
    translate_serial_output();

translate_external_call(_Mod, _Fun) ->
    %% Unknown external function - emit nop
    <<16#90>>.

%% Generate inline serial output code
%% Assumes RSI points to string (loaded by caller from put_string)
translate_serial_output() ->
    %% Call serial_puts helper
    %% We need to calculate relative offset to serial_puts
    %% This is tricky without multi-pass - for now, emit inline loop
    iolist_to_binary([
        %% Inline string output loop
        %% RSI = string pointer (assumed already loaded)
        <<16#AC>>,                                % lodsb
        <<16#84, 16#C0>>,                         % test al, al
        <<16#74, 16#0F>>,                         % jz done (+15 bytes)
        <<16#88, 16#C3>>,                         % mov bl, al
        %% Wait for TX ready
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,   % mov edx, 0x3FD
        <<16#EC>>,                                % in al, dx
        <<16#A8, 16#20>>,                         % test al, 0x20
        <<16#74, 16#F6>>,                         % jz wait (-10)
        <<16#88, 16#D8>>,                         % mov al, bl
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,   % mov edx, 0x3F8
        <<16#EE>>,                                % out dx, al
        <<16#EB, 16#E4>>,                         % jmp loop (-28)
        %% done:
        <<16#C3>>                                 % ret (placeholder)
    ]).

%% ============================================================================
%% String Embedding
%% ============================================================================

translate_put_string(_Len, String, {x, 0}) ->
    %% For bare metal, we need RIP-relative addressing
    %% Simplified: load string pointer (would embed string in data section)
    %% For now, just load 0 (strings would need multi-pass compilation)
    _ = String,  % Suppress warning
    <<16#48, 16#31, 16#C0>>;  % xor rax, rax (placeholder)

translate_put_string(_Len, _String, _Dst) ->
    <<16#90>>.  % nop
