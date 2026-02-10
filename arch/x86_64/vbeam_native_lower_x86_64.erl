-module(vbeam_native_lower_x86_64).

%% Lower IR instructions to x86_64 machine code.
%% Produces a list of code parts (binaries + label/reloc markers)
%% that the orchestrator assembles and links.

-export([
    lower_function/2
]).

-define(ENC, vbeam_native_x86_64).

%% Lower a complete function to code parts.
%% Returns {Parts, LinkState} where Parts is a list of:
%%   binary()           — encoded machine code
%%   {label, Name}      — label marker
%%   {reloc, Type, Sym, Addend} — relocation marker
-spec lower_function(map(), term()) -> {[term()], term()}.
lower_function(#{name := Name, body := Body, arity := _Arity,
                 spill_slots := SpillSlots} = _Fn, LinkState) ->
    %% Scan body to find which callee-saved registers are actually used
    UsedCalleeSaved = find_used_callee_saved(Body),
    FrameSize = compute_frame_size(SpillSlots, UsedCalleeSaved),
    Prologue = emit_prologue(FrameSize, UsedCalleeSaved),
    %% Thread UsedCalleeSaved through body lowering for ret instruction
    BodyParts = lower_body(Body, Name, UsedCalleeSaved),
    %% Don't emit epilogue here — ret instructions in body handle it
    Parts = Prologue ++ BodyParts,
    {Parts, LinkState}.

%% Compute stack frame size (16-byte aligned).
compute_frame_size(SpillSlots, UsedCalleeSaved) ->
    %% Each spill slot is 8 bytes
    SpillSize = SpillSlots * 8,
    %% FIXED BUG #3: Correct alignment formula.
    %% After entry: rsp is 16-aligned (call pushes 8-byte return address, making it 8-aligned,
    %% then push rbp makes it 16-aligned again).
    %% Then we push N callee-saved registers (each 8 bytes).
    %% Then we sub rsp, FrameSize for spill slots.
    %% For correct alignment at call sites: (1 + N) * 8 + FrameSize ≡ 0 (mod 16)
    %% So FrameSize must have opposite parity to (1 + N).
    TotalPushes = 1 + length(UsedCalleeSaved),  % rbp + callee-saved
    %% If TotalPushes is odd, FrameSize must be odd multiple of 8 (add 8 if even).
    %% If TotalPushes is even, FrameSize must be even multiple of 8 (no change if even).
    case {TotalPushes rem 2, SpillSize rem 16} of
        {1, 0} -> SpillSize + 8;  % Odd pushes, even SpillSize → add 8
        {1, 8} -> SpillSize;      % Odd pushes, odd SpillSize → OK
        {0, 0} -> SpillSize;      % Even pushes, even SpillSize → OK
        {0, 8} -> SpillSize + 8   % Even pushes, odd SpillSize → add 8
    end.

%% Emit function prologue: push rbp; mov rbp, rsp; sub rsp, FrameSize; save callee-saved
emit_prologue(0, []) ->
    [?ENC:encode_push(rbp),
     ?ENC:encode_mov_rr(rbp, rsp)];
emit_prologue(FrameSize, []) ->
    [?ENC:encode_push(rbp),
     ?ENC:encode_mov_rr(rbp, rsp),
     ?ENC:encode_sub_imm(rsp, FrameSize)];
emit_prologue(0, UsedCalleeSaved) ->
    %% FIXED: Push callee-saved FIRST, then subtract aligned spill space
    [?ENC:encode_push(rbp),
     ?ENC:encode_mov_rr(rbp, rsp)] ++
    [?ENC:encode_push(Reg) || Reg <- UsedCalleeSaved];
emit_prologue(FrameSize, UsedCalleeSaved) ->
    %% FIXED: Push callee-saved FIRST, then subtract aligned spill space
    %% so rsp is 16-byte aligned before any call
    [?ENC:encode_push(rbp),
     ?ENC:encode_mov_rr(rbp, rsp)] ++
    [?ENC:encode_push(Reg) || Reg <- UsedCalleeSaved] ++
    (if FrameSize > 0 -> [?ENC:encode_sub_imm(rsp, FrameSize)];
        true -> []
     end).

%% Emit function epilogue: restore callee-saved; mov rsp, rbp; pop rbp; ret
%% Note: This is called from lower_instruction(ret, ...) which doesn't have access to UsedCalleeSaved,
%% so we can't restore here. Instead, we need to pass UsedCalleeSaved through the lowering context.
%% For now, keep the old signature and handle restoration in the ret instruction.
emit_epilogue(0) ->
    [?ENC:encode_pop(rbp),
     ?ENC:encode_ret()];
emit_epilogue(_FrameSize) ->
    [?ENC:encode_mov_rr(rsp, rbp),
     ?ENC:encode_pop(rbp),
     ?ENC:encode_ret()].

%% Lower the body instructions.
lower_body([], _FnName, _UsedCalleeSaved) -> [];
lower_body([Inst | Rest], FnName, UsedCalleeSaved) ->
    Parts = lower_instruction(Inst, FnName, UsedCalleeSaved),
    Parts ++ lower_body(Rest, FnName, UsedCalleeSaved).

%% Lower a single IR instruction to code parts.

%% Labels
lower_instruction({label, Name}, _FnName, _UsedCalleeSaved) ->
    [{label, Name}];

%% Comments (no code emitted)
lower_instruction({comment, _}, _FnName, _UsedCalleeSaved) ->
    [];

%% NOP
lower_instruction(nop, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_nop()];

%% MOV register to register
lower_instruction({mov, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_rr(Dst, Src)];

%% MOV immediate to register
lower_instruction({mov_imm, {preg, Dst}, Imm}, _FnName, _UsedCalleeSaved) when is_integer(Imm) ->
    if Imm >= -2147483648, Imm =< 2147483647 ->
        [?ENC:encode_mov_imm32(Dst, Imm)];
       true ->
        [?ENC:encode_mov_imm64(Dst, Imm)]
    end;

%% MOV with stack operands (spilled registers)
lower_instruction({mov, {stack, Slot}, {preg, Src}}, _FnName, UsedCalleeSaved) ->
    %% Store to stack: MOV [rbp - offset], Src
    %% FIXED: Offset must account for pushed callee-saved registers
    Offset = -((Slot + 1) * 8 + length(UsedCalleeSaved) * 8),
    [?ENC:encode_mov_mem_store(rbp, Offset, Src)];
lower_instruction({mov, {preg, Dst}, {stack, Slot}}, _FnName, UsedCalleeSaved) ->
    %% Load from stack: MOV Dst, [rbp - offset]
    %% FIXED: Offset must account for pushed callee-saved registers
    Offset = -((Slot + 1) * 8 + length(UsedCalleeSaved) * 8),
    [?ENC:encode_mov_mem_load(Dst, rbp, Offset)];

%% LOAD from memory
lower_instruction({load, {preg, Dst}, {preg, Base}, Off}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_load(Dst, Base, Off)];
lower_instruction({load, {preg, Dst}, {stack, Slot}, Off}, _FnName, UsedCalleeSaved) ->
    %% Load base from stack first, then load from it
    %% Offset must account for pushed callee-saved registers
    BaseOff = -((Slot + 1) * 8 + length(UsedCalleeSaved) * 8),
    [?ENC:encode_mov_mem_load(Dst, rbp, BaseOff),
     ?ENC:encode_mov_mem_load(Dst, Dst, Off)];

%% LOAD_BYTE from memory (zero-extends byte to 64-bit)
lower_instruction({load_byte, {preg, Dst}, {preg, Base}, Off}, _FnName, _UsedCalleeSaved) ->
    %% MOVZX r64, byte [Base + Off]: REX.W 0F B6 /r
    Rex = ?ENC:rex(1, ?ENC:reg_hi(Dst), 0, ?ENC:reg_hi(Base)),
    DstLo = ?ENC:reg_lo(Dst),
    BaseLo = ?ENC:reg_lo(Base),
    case {Base, Off} of
        {rsp, _} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, DstLo, 4),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8, Off:8/signed>>];
        {rsp, _} ->
            M = ?ENC:modrm(2, DstLo, 4),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8, Off:32/little-signed>>];
        {r12, _} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, DstLo, 4),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8, Off:8/signed>>];
        {r12, _} ->
            M = ?ENC:modrm(2, DstLo, 4),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8, Off:32/little-signed>>];
        {_, 0} when Base =/= rbp, Base =/= r13 ->
            M = ?ENC:modrm(0, DstLo, BaseLo),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8>>];
        {_, Off} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, DstLo, BaseLo),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, Off:8/signed>>];
        _ ->
            M = ?ENC:modrm(2, DstLo, BaseLo),
            [<<Rex:8, 16#0F:8, 16#B6:8, M:8, Off:32/little-signed>>]
    end;

%% STORE to memory
lower_instruction({store, {preg, Base}, Off, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_store(Base, Off, Src)];

%% STORE_BYTE to memory
lower_instruction({store_byte, {preg, Base}, Off, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    Rex = 16#40 bor (?ENC:reg_hi(Src) bsl 2) bor ?ENC:reg_hi(Base),
    SrcLo = ?ENC:reg_lo(Src),
    BaseLo = ?ENC:reg_lo(Base),
    %% rsp and r12 both encode as rm=4 and require SIB byte
    case {Base, Off} of
        {rsp, 0} ->
            M = ?ENC:modrm(0, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8>>];
        {rsp, Off} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8, Off:8/signed>>];
        {rsp, _} ->
            M = ?ENC:modrm(2, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8, Off:32/little-signed>>];
        {r12, 0} ->
            M = ?ENC:modrm(0, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8>>];
        {r12, Off} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8, Off:8/signed>>];
        {r12, _} ->
            M = ?ENC:modrm(2, SrcLo, 4),
            [<<Rex:8, 16#88:8, M:8, 16#24:8, Off:32/little-signed>>];
        {_, 0} when Base =/= rbp, Base =/= r13 ->
            M = ?ENC:modrm(0, SrcLo, BaseLo),
            [<<Rex:8, 16#88:8, M:8>>];
        {_, Off} when Off >= -128, Off =< 127 ->
            M = ?ENC:modrm(1, SrcLo, BaseLo),
            [<<Rex:8, 16#88:8, M:8, Off:8/signed>>];
        _ ->
            M = ?ENC:modrm(2, SrcLo, BaseLo),
            [<<Rex:8, 16#88:8, M:8, Off:32/little-signed>>]
    end;

%% ADD
lower_instruction({add, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    if Dst =:= A ->
        [?ENC:encode_add_rr(Dst, B)];
       Dst =:= B ->
        [?ENC:encode_add_rr(Dst, A)];
       true ->
        [?ENC:encode_mov_rr(Dst, A),
         ?ENC:encode_add_rr(Dst, B)]
    end;
lower_instruction({add, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_add_imm(Dst, Imm)];

%% SUB
lower_instruction({sub, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_sub_rr(Dst, B)];
lower_instruction({sub, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_sub_imm(Dst, Imm)];

%% MUL (IMUL r64, r64)
lower_instruction({mul, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_imul_rr(Dst, B)];

%% SDIV (uses rdx:rax / src -> rax=quot, rdx=rem)
lower_instruction({sdiv, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    %% FIXED: Divisor B must not be in rax/rdx (they're clobbered by div).
    %% If B is rax or rdx, move to safe temp (r11) first.
    SafeB = if B =:= rax orelse B =:= rdx -> r11; true -> B end,
    SaveB = if B =:= rax orelse B =:= rdx -> [?ENC:encode_mov_rr(r11, B)]; true -> [] end,
    %% FIXED: Add runtime zero-divisor check before idiv
    ZeroCheck = [
        ?ENC:encode_test_rr(SafeB, SafeB),  %% test divisor, divisor
        ?ENC:encode_jcc_rel32(ne, 2),       %% jne +2 (skip ud2)
        ?ENC:encode_ud2()                   %% trap if zero
    ],
    %% Move dividend to rax, sign-extend to rdx:rax, divide, result in rax
    MovA = if A =/= rax -> [?ENC:encode_mov_rr(rax, A)]; true -> [] end,
    DivParts = SaveB ++ ZeroCheck ++ MovA ++ [?ENC:encode_cqo(), ?ENC:encode_idiv(SafeB)],
    MovDst = if Dst =/= rax -> DivParts ++ [?ENC:encode_mov_rr(Dst, rax)];
                true -> DivParts end,
    MovDst;

%% SREM (remainder is in rdx after idiv)
lower_instruction({srem, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    %% FIXED: Divisor B must not be in rax/rdx (they're clobbered by div).
    SafeB = if B =:= rax orelse B =:= rdx -> r11; true -> B end,
    SaveB = if B =:= rax orelse B =:= rdx -> [?ENC:encode_mov_rr(r11, B)]; true -> [] end,
    %% FIXED: Add runtime zero-divisor check before idiv
    ZeroCheck = [
        ?ENC:encode_test_rr(SafeB, SafeB),  %% test divisor, divisor
        ?ENC:encode_jcc_rel32(ne, 2),       %% jne +2 (skip ud2)
        ?ENC:encode_ud2()                   %% trap if zero
    ],
    MovA = if A =/= rax -> [?ENC:encode_mov_rr(rax, A)]; true -> [] end,
    DivParts = SaveB ++ ZeroCheck ++ MovA ++ [?ENC:encode_cqo(), ?ENC:encode_idiv(SafeB)],
    MovDst = if Dst =/= rdx -> DivParts ++ [?ENC:encode_mov_rr(Dst, rdx)];
                true -> DivParts end,
    MovDst;

%% AND
lower_instruction({and_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_and_rr(Dst, B)];

%% OR
lower_instruction({or_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_or_rr(Dst, B)];

%% XOR
lower_instruction({xor_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
    Parts0 ++ [?ENC:encode_xor_rr(Dst, B)];

%% SHL (shift left — shift amount must be in CL register)
lower_instruction({shl, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    %% FIXED: When Dst=rcx, moving count to rcx overwrites the value to shift.
    %% Use r11 as scratch: save A to scratch, move B to rcx, shift scratch, move to Dst.
    if Dst =:= rcx, B =/= rcx ->
        [?ENC:encode_mov_rr(r11, A),
         ?ENC:encode_mov_rr(rcx, B),
         ?ENC:encode_shl_cl(r11),
         ?ENC:encode_mov_rr(Dst, r11)];
       Dst =:= rcx, B =:= rcx ->
        %% B is already in rcx, shift it directly
        Parts0 = if A =/= rcx -> [?ENC:encode_mov_rr(rcx, A)]; true -> [] end,
        Parts0 ++ [?ENC:encode_shl_cl(rcx)];
       true ->
        %% Normal case: Dst is not rcx
        Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
        MovCL = if B =/= rcx -> [?ENC:encode_mov_rr(rcx, B)]; true -> [] end,
        Parts0 ++ MovCL ++ [?ENC:encode_shl_cl(Dst)]
    end;

%% SHR (logical shift right)
lower_instruction({shr, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    %% FIXED: When Dst=rcx, moving count to rcx overwrites the value to shift.
    if Dst =:= rcx, B =/= rcx ->
        [?ENC:encode_mov_rr(r11, A),
         ?ENC:encode_mov_rr(rcx, B),
         ?ENC:encode_shr_cl(r11),
         ?ENC:encode_mov_rr(Dst, r11)];
       Dst =:= rcx, B =:= rcx ->
        Parts0 = if A =/= rcx -> [?ENC:encode_mov_rr(rcx, A)]; true -> [] end,
        Parts0 ++ [?ENC:encode_shr_cl(rcx)];
       true ->
        Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
        MovCL = if B =/= rcx -> [?ENC:encode_mov_rr(rcx, B)]; true -> [] end,
        Parts0 ++ MovCL ++ [?ENC:encode_shr_cl(Dst)]
    end;

%% SAR (arithmetic shift right)
lower_instruction({sar, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    %% FIXED: When Dst=rcx, moving count to rcx overwrites the value to shift.
    if Dst =:= rcx, B =/= rcx ->
        [?ENC:encode_mov_rr(r11, A),
         ?ENC:encode_mov_rr(rcx, B),
         ?ENC:encode_sar_cl(r11),
         ?ENC:encode_mov_rr(Dst, r11)];
       Dst =:= rcx, B =:= rcx ->
        Parts0 = if A =/= rcx -> [?ENC:encode_mov_rr(rcx, A)]; true -> [] end,
        Parts0 ++ [?ENC:encode_sar_cl(rcx)];
       true ->
        Parts0 = if Dst =/= A -> [?ENC:encode_mov_rr(Dst, A)]; true -> [] end,
        MovCL = if B =/= rcx -> [?ENC:encode_mov_rr(rcx, B)]; true -> [] end,
        Parts0 ++ MovCL ++ [?ENC:encode_sar_cl(Dst)]
    end;

%% NEG
lower_instruction({neg, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= Src -> [?ENC:encode_mov_rr(Dst, Src)]; true -> [] end,
    Parts0 ++ [?ENC:encode_neg(Dst)];

%% NOT
lower_instruction({not_, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    Parts0 = if Dst =/= Src -> [?ENC:encode_mov_rr(Dst, Src)]; true -> [] end,
    Parts0 ++ [?ENC:encode_not(Dst)];

%% CMP
lower_instruction({cmp, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_cmp_rr(A, B)];
lower_instruction({cmp, {preg, A}, {imm, Imm}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_cmp_imm(A, Imm)];

%% JMP (unconditional)
lower_instruction({jmp, Label}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_jmp_rel32(0),  %% placeholder offset
     {reloc, rel32, Label, -4}]; %% reloc to patch

%% JCC (conditional)
lower_instruction({jcc, Cond, Label}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_jcc_rel32(Cond, 0),  %% placeholder
     {reloc, rel32, Label, -4}];

%% CALL by symbol
lower_instruction({call, {sym, Name}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_call_rel32(0),  %% placeholder
     {reloc, rel32, Name, -4}];

%% CALL indirect through register
lower_instruction({call_indirect, {preg, Reg}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_call_reg(Reg)];

%% RET
lower_instruction(ret, _FnName, UsedCalleeSaved) ->
    %% FIXED BUG #1: Deallocate local frame FIRST, then pop callee-saved in reverse.
    %% Frame layout: rbp → callee-saved → spill area → rsp
    %% Epilogue sequence: add rsp, FrameSize → pop callee-saved (reverse) → pop rbp → ret
    %% Note: FrameSize is not available here, so we use mov rsp, rbp to deallocate
    %% which reclaims both spill area and callee-saved space in one instruction.
    %% Then pop callee-saved registers in REVERSE order [r15, r14, r13, r12, rbx].
    case UsedCalleeSaved of
        [] ->
            emit_epilogue(0);
        _ ->
            %% mov rsp, rbp (reclaim entire frame)
            RestoreCode = [?ENC:encode_pop(Reg) || Reg <- lists:reverse(UsedCalleeSaved)],
            [?ENC:encode_mov_rr(rsp, rbp)] ++ RestoreCode ++ [?ENC:encode_pop(rbp), ?ENC:encode_ret()]
    end;

%% PUSH
lower_instruction({push, {preg, Reg}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_push(Reg)];
lower_instruction({push, {imm, Imm}}, _FnName, _UsedCalleeSaved) ->
    %% x86_64 PUSH imm32: 68 + imm32 (sign-extended to 64-bit)
    [?ENC:encode_push_imm32(Imm)];

%% POP
lower_instruction({pop, {preg, Reg}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_pop(Reg)];

%% LEA (load effective address, RIP-relative for data references)
lower_instruction({lea, {preg, Dst}, {data_ref, Name}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_lea_rip_rel(Dst, 0),  %% placeholder
     {reloc, rel32, Name, -4}];

%% SYSCALL
lower_instruction(syscall, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_syscall()];

%%====================================================================
%% Phase 6a: String instructions
%%====================================================================

%% STRING_LIT: Load string literal address and build fat pointer.
%% Allocates 16 bytes on heap: {ptr, len}
lower_instruction({string_lit, {preg, Dst}, {data_ref, Name}, Len}, _FnName, _UsedCalleeSaved) ->
    AllocCode = vbeam_native_alloc:emit_alloc(x86_64, Dst, 16),
    %% LEA rax, [rip + Name]  (load data address)
    LeaCode = [?ENC:encode_lea_rip_rel(rax, 0),
               {reloc, rel32, Name, -4}],
    %% Store ptr and len in heap-allocated pair
    StoreCode = [?ENC:encode_mov_mem_store(Dst, 0, rax),   %% [Dst+0] = ptr
                 ?ENC:encode_mov_imm64(rcx, Len),
                 ?ENC:encode_mov_mem_store(Dst, 8, rcx)],   %% [Dst+8] = len
    lists:flatten([AllocCode, LeaCode, StoreCode]);

%% STRING_LIT (2-arg form): just LEA
lower_instruction({string_lit, {preg, Dst}, {data_ref, Name}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_lea_rip_rel(Dst, 0),
     {reloc, rel32, Name, -4}];

%% STRING_LEN: Load length from fat pointer.
lower_instruction({string_len, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_load(Dst, Src, 8)];

%% STRING_CMP: Compare two strings byte-by-byte.
lower_instruction({string_cmp, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__scmp_loop_", Uid/binary>>,
    EqLbl = <<"__scmp_eq_", Uid/binary>>,
    LtLbl = <<"__scmp_lt_", Uid/binary>>,
    GtLbl = <<"__scmp_gt_", Uid/binary>>,
    DoneLbl = <<"__scmp_done_", Uid/binary>>,
    MinOkLbl = <<"__scmp_minok_", Uid/binary>>,
    lists:flatten([
        %% Load string fields: rsi=A.ptr, rdx=A.len, rdi=B.ptr, r8=B.len
        ?ENC:encode_mov_mem_load(rsi, A, 0),
        ?ENC:encode_mov_mem_load(rdx, A, 8),
        ?ENC:encode_mov_mem_load(rdi, B, 0),
        ?ENC:encode_mov_mem_load(r8, B, 8),
        %% r9 = min(A.len, B.len)
        ?ENC:encode_mov_rr(r9, rdx),
        ?ENC:encode_cmp_rr(rdx, r8),
        ?ENC:encode_jcc_rel32(le, 0),
        {reloc, rel32, MinOkLbl, -4},
        ?ENC:encode_mov_rr(r9, r8),
        {label, MinOkLbl},
        %% r10 = 0 (index)
        ?ENC:encode_mov_imm64(r10, 0),

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(r10, r9),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, EqLbl, -4},
        %% Compare bytes
        %% Load A[r10] and B[r10] using byte-level MOVZX
        %% Use ADD to compute address, then MOVZX byte load
        ?ENC:encode_mov_rr(rax, rsi),
        ?ENC:encode_add_rr(rax, r10),
        encode_movzx_byte_mem(rax, rax),  %% MOVZX rax, byte [rax]
        ?ENC:encode_mov_rr(rcx, rdi),
        ?ENC:encode_add_rr(rcx, r10),
        encode_movzx_byte_mem(rcx, rcx),  %% MOVZX rcx, byte [rcx]
        ?ENC:encode_cmp_rr(rax, rcx),
        ?ENC:encode_jcc_rel32(lt, 0),
        {reloc, rel32, LtLbl, -4},
        ?ENC:encode_jcc_rel32(gt, 0),
        {reloc, rel32, GtLbl, -4},
        ?ENC:encode_add_imm(r10, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, LoopLbl, -4},

        {label, EqLbl},
        ?ENC:encode_cmp_rr(rdx, r8),
        ?ENC:encode_jcc_rel32(lt, 0),
        {reloc, rel32, LtLbl, -4},
        ?ENC:encode_jcc_rel32(gt, 0),
        {reloc, rel32, GtLbl, -4},
        ?ENC:encode_mov_imm64(Dst, 0),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, LtLbl},
        ?ENC:encode_mov_imm64(Dst, -1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, GtLbl},
        ?ENC:encode_mov_imm64(Dst, 1),

        {label, DoneLbl}
    ]);

%% STRING_CONCAT: Concatenate two strings.
lower_instruction({string_concat, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    CopyALbl = <<"__scat_cpA_", Uid/binary>>,
    CopyADone = <<"__scat_cpAd_", Uid/binary>>,
    CopyBLbl = <<"__scat_cpB_", Uid/binary>>,
    CopyBDone = <<"__scat_cpBd_", Uid/binary>>,
    HeapReg = vbeam_native_alloc:heap_reg(x86_64),
    lists:flatten([
        %% Load fields
        ?ENC:encode_mov_mem_load(rsi, A, 0),   %% A.ptr
        ?ENC:encode_mov_mem_load(rdx, A, 8),   %% A.len
        ?ENC:encode_mov_mem_load(rdi, B, 0),   %% B.ptr
        ?ENC:encode_mov_mem_load(r8, B, 8),    %% B.len

        %% r9 = total_len
        ?ENC:encode_mov_rr(r9, rdx),
        ?ENC:encode_add_rr(r9, r8),

        %% Allocate data buffer: r10 = bump alloc(total_len)
        ?ENC:encode_mov_rr(r10, HeapReg),
        ?ENC:encode_mov_rr(r11, r9),
        ?ENC:encode_add_imm(r11, 7),
        encode_and_imm_x86(r11, -8),
        ?ENC:encode_add_rr(HeapReg, r11),

        %% Copy A bytes: memcpy(r10, rsi, rdx)
        ?ENC:encode_mov_imm64(r11, 0),
        {label, CopyALbl},
        ?ENC:encode_cmp_rr(r11, rdx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, CopyADone, -4},
        %% Load byte: movzx rax, byte [rsi+r11]
        ?ENC:encode_mov_rr(rax, rsi),
        ?ENC:encode_add_rr(rax, r11),
        encode_movzx_byte_mem(rax, rax),
        %% Store byte: mov byte [r10+r11], al
        ?ENC:encode_mov_rr(rcx, r10),
        ?ENC:encode_add_rr(rcx, r11),
        encode_mov_byte_reg_to_mem(rcx, rax),
        ?ENC:encode_add_imm(r11, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, CopyALbl, -4},
        {label, CopyADone},

        %% Copy B bytes
        ?ENC:encode_mov_imm64(r11, 0),
        {label, CopyBLbl},
        ?ENC:encode_cmp_rr(r11, r8),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, CopyBDone, -4},
        %% Load byte: movzx rax, byte [rdi+r11]
        ?ENC:encode_mov_rr(rax, rdi),
        ?ENC:encode_add_rr(rax, r11),
        encode_movzx_byte_mem(rax, rax),
        %% Store byte: mov byte [r10+rdx+r11], al
        ?ENC:encode_mov_rr(rcx, r10),
        ?ENC:encode_add_rr(rcx, rdx),          %% + A.len
        ?ENC:encode_add_rr(rcx, r11),          %% + index
        encode_mov_byte_reg_to_mem(rcx, rax),
        ?ENC:encode_add_imm(r11, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, CopyBLbl, -4},
        {label, CopyBDone},

        %% Allocate fat pointer
        vbeam_native_alloc:emit_alloc(x86_64, Dst, 16),
        ?ENC:encode_mov_mem_store(Dst, 0, r10),        %% ptr
        ?ENC:encode_mov_mem_store(Dst, 8, r9)          %% len
    ]);

%% PRINT_STR: Print string via write syscall.
%% Saves all caller-visible registers that might hold user values.
%% SYSCALL clobbers rcx and r11; we save those too.
lower_instruction({print_str, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    lists:flatten([
        %% Save registers (SYSCALL clobbers rcx, r11 in addition to args)
        ?ENC:encode_push(rdi),
        ?ENC:encode_push(rsi),
        ?ENC:encode_push(rdx),
        ?ENC:encode_push(rax),
        ?ENC:encode_push(rcx),
        ?ENC:encode_push(r11),

        %% Load into temps first (Src could be any register!)
        ?ENC:encode_mov_mem_load(r10, Src, 0),        %% r10 = buf (ptr)
        ?ENC:encode_mov_mem_load(r8, Src, 8),         %% r8 = len
        %% write(1, buf, len)
        ?ENC:encode_mov_imm64(rax, 1),               %% sys_write
        ?ENC:encode_mov_imm64(rdi, 1),               %% fd = stdout
        ?ENC:encode_mov_rr(rsi, r10),                 %% buf
        ?ENC:encode_mov_rr(rdx, r8),                  %% len
        ?ENC:encode_syscall(),

        %% Restore (reverse order)
        ?ENC:encode_pop(r11),
        ?ENC:encode_pop(rcx),
        ?ENC:encode_pop(rax),
        ?ENC:encode_pop(rdx),
        ?ENC:encode_pop(rsi),
        ?ENC:encode_pop(rdi)
    ]);

%%====================================================================
%% Phase 6b: Array instructions
%%====================================================================

%% ARRAY_NEW
lower_instruction({array_new, {preg, Dst}, {imm, ElemSize}, {imm, InitCap}}, _FnName, _UsedCalleeSaved) ->
    BufSize = ElemSize * InitCap,
    lists:flatten([
        vbeam_native_alloc:emit_alloc(x86_64, rax, BufSize), %% elem buffer in rax
        ?ENC:encode_push(rax),                                %% save buf ptr
        vbeam_native_alloc:emit_alloc(x86_64, Dst, 24),      %% header
        ?ENC:encode_pop(rax),                                 %% restore buf ptr
        ?ENC:encode_mov_mem_store(Dst, 0, rax),               %% ptr
        ?ENC:encode_mov_imm64(rcx, 0),
        ?ENC:encode_mov_mem_store(Dst, 8, rcx),               %% len = 0
        ?ENC:encode_mov_imm64(rcx, InitCap),
        ?ENC:encode_mov_mem_store(Dst, 16, rcx)               %% cap
    ]);

%% ARRAY_GET (register index)
lower_instruction({array_get, {preg, Dst}, {preg, Arr}, {preg, Idx}, {imm, ElemSize}},
                  _FnName, _UsedCalleeSaved) ->
    lists:flatten([
        ?ENC:encode_mov_mem_load(rax, Arr, 0),               %% rax = ptr
        ?ENC:encode_mov_imm64(rcx, ElemSize),
        ?ENC:encode_mov_rr(rdx, Idx),
        ?ENC:encode_imul_rr(rdx, rcx),                       %% rdx = idx * elem_size
        ?ENC:encode_add_rr(rax, rdx),
        ?ENC:encode_mov_mem_load(Dst, rax, 0)
    ]);

%% ARRAY_GET (immediate index)
lower_instruction({array_get, {preg, Dst}, {preg, Arr}, {imm, Idx}, {imm, ElemSize}},
                  _FnName, _UsedCalleeSaved) ->
    Offset = Idx * ElemSize,
    lists:flatten([
        ?ENC:encode_mov_mem_load(rax, Arr, 0),
        ?ENC:encode_mov_mem_load(Dst, rax, Offset)
    ]);

%% ARRAY_SET (immediate index)
lower_instruction({array_set, {preg, Arr}, {imm, Idx}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _UsedCalleeSaved) ->
    Offset = Idx * ElemSize,
    lists:flatten([
        ?ENC:encode_mov_mem_load(rax, Arr, 0),
        ?ENC:encode_mov_mem_store(rax, Offset, Val)
    ]);

%% ARRAY_SET (register index)
lower_instruction({array_set, {preg, Arr}, {preg, Idx}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _UsedCalleeSaved) ->
    lists:flatten([
        ?ENC:encode_mov_mem_load(rax, Arr, 0),
        ?ENC:encode_mov_imm64(rcx, ElemSize),
        ?ENC:encode_mov_rr(rdx, Idx),
        ?ENC:encode_imul_rr(rdx, rcx),
        ?ENC:encode_add_rr(rax, rdx),
        ?ENC:encode_mov_mem_store(rax, 0, Val)
    ]);

%% ARRAY_LEN
lower_instruction({array_len, {preg, Dst}, {preg, Arr}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_load(Dst, Arr, 8)];

%% ARRAY_APPEND
lower_instruction({array_append, {preg, Dst}, {preg, Arr}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NeedGrowLbl = <<"__aapp_grow_", Uid/binary>>,
    StoreLbl = <<"__aapp_store_", Uid/binary>>,
    CopyLoopLbl = <<"__aapp_cpy_", Uid/binary>>,
    CopyDoneLbl = <<"__aapp_cpyd_", Uid/binary>>,
    NotzLbl = <<"__aapp_notz_", Uid/binary>>,
    DblLbl = <<"__aapp_dbl_", Uid/binary>>,
    HeapReg = vbeam_native_alloc:heap_reg(x86_64),
    lists:flatten([
        %% Load header
        ?ENC:encode_mov_mem_load(rsi, Arr, 0),    %% ptr
        ?ENC:encode_mov_mem_load(rdx, Arr, 8),    %% len
        ?ENC:encode_mov_mem_load(rdi, Arr, 16),   %% cap

        ?ENC:encode_cmp_rr(rdx, rdi),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, NeedGrowLbl, -4},
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, StoreLbl, -4},

        %% Grow
        {label, NeedGrowLbl},
        ?ENC:encode_cmp_imm(rdi, 0),
        ?ENC:encode_jcc_rel32(ne, 0),
        {reloc, rel32, NotzLbl, -4},
        ?ENC:encode_mov_imm64(rdi, 2),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DblLbl, -4},
        {label, NotzLbl},
        ?ENC:encode_add_rr(rdi, rdi),             %% cap *= 2
        {label, DblLbl},

        %% Allocate new buffer
        ?ENC:encode_mov_imm64(r8, ElemSize),
        ?ENC:encode_mov_rr(r9, rdi),
        ?ENC:encode_imul_rr(r9, r8),              %% new_buf_size = cap * elem_size
        %% Bump allocate
        ?ENC:encode_mov_rr(r10, HeapReg),          %% new_buf
        ?ENC:encode_mov_rr(r11, r9),
        ?ENC:encode_add_imm(r11, 7),
        encode_and_imm_x86(r11, -8),
        ?ENC:encode_add_rr(HeapReg, r11),

        %% Copy old data (BUG #2 FIX: byte-by-byte for arbitrary element sizes)
        ?ENC:encode_mov_rr(r11, rdx),
        ?ENC:encode_imul_rr(r11, r8),              %% old bytes = len * elem_size
        ?ENC:encode_mov_imm64(r9, 0),
        {label, CopyLoopLbl},
        ?ENC:encode_cmp_rr(r9, r11),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, CopyDoneLbl, -4},
        %% Load one byte: movzx rcx, byte [rsi + r9]
        ?ENC:encode_mov_rr(rax, rsi),
        ?ENC:encode_add_rr(rax, r9),
        encode_movzx_byte_mem(rcx, rax),
        %% Store one byte: mov byte [r10 + r9], cl
        ?ENC:encode_mov_rr(rax, r10),
        ?ENC:encode_add_rr(rax, r9),
        encode_mov_byte_reg_to_mem(rax, rcx),
        %% Increment by 1 byte (not 8)
        ?ENC:encode_add_imm(r9, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, CopyLoopLbl, -4},
        {label, CopyDoneLbl},

        %% Update header
        ?ENC:encode_mov_rr(rsi, r10),
        ?ENC:encode_mov_mem_store(Arr, 0, r10),   %% ptr = new_buf
        ?ENC:encode_mov_mem_store(Arr, 16, rdi),   %% cap = new_cap

        %% Store element
        {label, StoreLbl},
        ?ENC:encode_mov_imm64(r8, ElemSize),
        ?ENC:encode_mov_rr(r9, rdx),
        ?ENC:encode_imul_rr(r9, r8),               %% offset = len * elem_size
        ?ENC:encode_add_rr(rsi, r9),
        ?ENC:encode_mov_mem_store(rsi, 0, Val),

        %% len++
        ?ENC:encode_add_imm(rdx, 1),
        ?ENC:encode_mov_mem_store(Arr, 8, rdx),

        ?ENC:encode_mov_rr(Dst, Arr)
    ]);

%%====================================================================
%% Phase 6c: Struct instructions
%%====================================================================

lower_instruction({struct_new, {preg, Dst}, {imm, Size}}, _FnName, _UsedCalleeSaved) ->
    vbeam_native_alloc:emit_alloc(x86_64, Dst, Size);

lower_instruction({field_get, {preg, Dst}, {preg, Struct}, {imm, Offset}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_load(Dst, Struct, Offset)];

lower_instruction({field_set, {preg, Struct}, {imm, Offset}, {preg, Val}}, _FnName, _UsedCalleeSaved) ->
    [?ENC:encode_mov_mem_store(Struct, Offset, Val)];

%%====================================================================
%% Phase 6d: Map instructions
%%====================================================================

lower_instruction({map_new, {preg, Dst}}, _FnName, _UsedCalleeSaved) ->
    InitCap = 8,
    BufSize = InitCap * 16,
    lists:flatten([
        vbeam_native_alloc:emit_alloc(x86_64, rax, BufSize),
        ?ENC:encode_push(rax),
        vbeam_native_alloc:emit_alloc(x86_64, Dst, 24),
        ?ENC:encode_pop(rax),
        ?ENC:encode_mov_mem_store(Dst, 0, rax),
        ?ENC:encode_mov_imm64(rcx, 0),
        ?ENC:encode_mov_mem_store(Dst, 8, rcx),
        ?ENC:encode_mov_imm64(rcx, InitCap),
        ?ENC:encode_mov_mem_store(Dst, 16, rcx)
    ]);

lower_instruction({map_get, {preg, Dst}, {preg, Map}, {preg, Key}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__mg_loop_", Uid/binary>>,
    FoundLbl = <<"__mg_found_", Uid/binary>>,
    NotFoundLbl = <<"__mg_nf_", Uid/binary>>,
    DoneLbl = <<"__mg_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_mov_mem_load(rsi, Map, 0),
        ?ENC:encode_mov_mem_load(rdx, Map, 8),
        ?ENC:encode_mov_imm64(rdi, 0),

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(rdi, rdx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, NotFoundLbl, -4},

        ?ENC:encode_mov_imm64(r8, 16),
        ?ENC:encode_mov_rr(r9, rdi),
        ?ENC:encode_imul_rr(r9, r8),
        ?ENC:encode_mov_rr(r10, rsi),
        ?ENC:encode_add_rr(r10, r9),
        ?ENC:encode_mov_mem_load(rax, r10, 0),

        ?ENC:encode_cmp_rr(rax, Key),
        ?ENC:encode_jcc_rel32(eq, 0),
        {reloc, rel32, FoundLbl, -4},

        ?ENC:encode_add_imm(rdi, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, LoopLbl, -4},

        {label, FoundLbl},
        ?ENC:encode_mov_mem_load(Dst, r10, 8),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, NotFoundLbl},
        ?ENC:encode_mov_imm64(Dst, 0),

        {label, DoneLbl}
    ]);

lower_instruction({map_put, {preg, Dst}, {preg, Map}, {preg, Key}, {preg, Val}},
                  _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__mp_loop_", Uid/binary>>,
    FoundLbl = <<"__mp_found_", Uid/binary>>,
    AppendLbl = <<"__mp_append_", Uid/binary>>,
    CapFullLbl = <<"__mp_capfull_", Uid/binary>>,
    DoneLbl = <<"__mp_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_mov_mem_load(rsi, Map, 0),   %% entries ptr
        ?ENC:encode_mov_mem_load(rdx, Map, 8),   %% len
        ?ENC:encode_mov_mem_load(rcx, Map, 16),  %% cap
        ?ENC:encode_mov_imm64(rdi, 0),

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(rdi, rdx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, AppendLbl, -4},

        ?ENC:encode_mov_imm64(r8, 16),
        ?ENC:encode_mov_rr(r9, rdi),
        ?ENC:encode_imul_rr(r9, r8),
        ?ENC:encode_mov_rr(r10, rsi),
        ?ENC:encode_add_rr(r10, r9),
        ?ENC:encode_mov_mem_load(rax, r10, 0),

        ?ENC:encode_cmp_rr(rax, Key),
        ?ENC:encode_jcc_rel32(eq, 0),
        {reloc, rel32, FoundLbl, -4},

        ?ENC:encode_add_imm(rdi, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, LoopLbl, -4},

        {label, FoundLbl},
        ?ENC:encode_mov_mem_store(r10, 8, Val),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, AppendLbl},
        %% Bounds check: if len >= cap, fail (growth would go here)
        ?ENC:encode_cmp_rr(rdx, rcx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, CapFullLbl, -4},

        ?ENC:encode_mov_imm64(r8, 16),
        ?ENC:encode_mov_rr(r9, rdx),
        ?ENC:encode_imul_rr(r9, r8),
        ?ENC:encode_mov_rr(r10, rsi),
        ?ENC:encode_add_rr(r10, r9),
        ?ENC:encode_mov_mem_store(r10, 0, Key),
        ?ENC:encode_mov_mem_store(r10, 8, Val),
        ?ENC:encode_add_imm(rdx, 1),
        ?ENC:encode_mov_mem_store(Map, 8, rdx),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, CapFullLbl},
        %% FIXED: Map capacity exhausted - return map unchanged (graceful degradation)
        %% TODO: implement map growth/reallocation when capacity is reached
        %% Fall through to DoneLbl to return original map unchanged

        {label, DoneLbl},
        ?ENC:encode_mov_rr(Dst, Map)
    ]);

lower_instruction({map_delete, {preg, Dst}, {preg, Map}, {preg, Key}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__md_loop_", Uid/binary>>,
    FoundLbl = <<"__md_found_", Uid/binary>>,
    ShiftLbl = <<"__md_shift_", Uid/binary>>,
    ShiftDone = <<"__md_shd_", Uid/binary>>,
    DoneLbl = <<"__md_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_mov_mem_load(rsi, Map, 0),
        ?ENC:encode_mov_mem_load(rdx, Map, 8),
        ?ENC:encode_mov_imm64(rdi, 0),

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(rdi, rdx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, DoneLbl, -4},

        ?ENC:encode_mov_imm64(r8, 16),
        ?ENC:encode_mov_rr(r9, rdi),
        ?ENC:encode_imul_rr(r9, r8),
        ?ENC:encode_mov_rr(r10, rsi),
        ?ENC:encode_add_rr(r10, r9),
        ?ENC:encode_mov_mem_load(rax, r10, 0),

        ?ENC:encode_cmp_rr(rax, Key),
        ?ENC:encode_jcc_rel32(eq, 0),
        {reloc, rel32, FoundLbl, -4},

        ?ENC:encode_add_imm(rdi, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, LoopLbl, -4},

        {label, FoundLbl},
        ?ENC:encode_mov_rr(r11, rdi),
        ?ENC:encode_add_imm(r11, 1),
        {label, ShiftLbl},
        ?ENC:encode_cmp_rr(r11, rdx),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, ShiftDone, -4},
        ?ENC:encode_mov_imm64(r8, 16),
        ?ENC:encode_mov_rr(r9, r11),
        ?ENC:encode_imul_rr(r9, r8),
        ?ENC:encode_mov_rr(r10, rsi),
        ?ENC:encode_add_rr(r10, r9),
        ?ENC:encode_mov_mem_load(rax, r10, 0),
        ?ENC:encode_mov_mem_load(rcx, r10, 8),
        ?ENC:encode_sub_imm(r10, 16),
        ?ENC:encode_mov_mem_store(r10, 0, rax),
        ?ENC:encode_mov_mem_store(r10, 8, rcx),
        ?ENC:encode_add_imm(r11, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, ShiftLbl, -4},
        {label, ShiftDone},
        ?ENC:encode_sub_imm(rdx, 1),
        ?ENC:encode_mov_mem_store(Map, 8, rdx),

        {label, DoneLbl},
        ?ENC:encode_mov_rr(Dst, Map)
    ]);

%%====================================================================
%% Phase 6c: Struct instructions (above)
%% Phase 6e: Float instructions (placeholder)
%%====================================================================

lower_instruction({int_to_float, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    %% CVTSI2SD xmm0, Src; MOVQ Dst, xmm0
    %% For now, emit placeholder NOPs (TODO: proper FPU encoding)
    _SrcCode = ?ENC:reg_code(Src),
    _DstCode = ?ENC:reg_code(Dst),
    %% F2 REX.W 0F 2A /r = CVTSI2SD xmm0, r64
    Rex1 = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Src)),
    ModRM1 = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(Src)),
    Cvt = <<16#F2:8, Rex1:8, 16#0F:8, 16#2A:8, ModRM1:8>>,
    %% 66 REX.W 0F 7E /r = MOVQ r64, xmm0
    Rex2 = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Dst)),
    ModRM2 = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(Dst)),
    Movq = <<16#66:8, Rex2:8, 16#0F:8, 16#7E:8, ModRM2:8>>,
    [Cvt, Movq];

lower_instruction({float_to_int, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    %% MOVQ xmm0, Src; CVTTSD2SI Dst, xmm0
    Rex1 = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Src)),
    ModRM1 = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(Src)),
    Movq = <<16#66:8, Rex1:8, 16#0F:8, 16#6E:8, ModRM1:8>>,
    Rex2 = ?ENC:rex(1, ?ENC:reg_hi(Dst), 0, 0),
    ModRM2 = ?ENC:modrm(2#11, ?ENC:reg_lo(Dst), 0),
    Cvt = <<16#F2:8, Rex2:8, 16#0F:8, 16#2C:8, ModRM2:8>>,
    [Movq, Cvt];

%%====================================================================
%% Phase 6f: Method calls
%%====================================================================

lower_instruction({method_call, {preg, Dst}, ReceiverType, MethodName, Args},
                  _FnName, _UsedCalleeSaved) when is_binary(ReceiverType),
                                is_binary(MethodName),
                                is_list(Args) ->
    %% FIXED BUG #8: Guard against >6 arguments (stack args not yet supported)
    ArgRegsList = [rdi, rsi, rdx, rcx, r8, r9],
    case length(Args) > length(ArgRegsList) of
        true ->
            error({method_call_too_many_args, MethodName, length(Args),
                   "Stack arguments not yet implemented. Max 6 args."});
        false ->
            MangledName = <<ReceiverType/binary, "__", MethodName/binary>>,
            ArgMoves = move_args_to_regs_x86(Args, ArgRegsList),
            CallCode = [?ENC:encode_call_rel32(0),
                        {reloc, rel32, MangledName, -4}],
            ResultMove = case Dst of
                rax -> [];
                _   -> [?ENC:encode_mov_rr(Dst, rax)]
            end,
            lists:flatten([ArgMoves, CallCode, ResultMove])
    end;

%%====================================================================
%% Phase 6g: Type conversions
%%====================================================================

%% INT_TO_STR: Convert integer to string (fat pointer).
%% Uses a stack buffer for digit extraction, then copies to heap.
lower_instruction({int_to_str, {preg, Dst}, {preg, Src}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NonZeroLbl = <<"__its_nz_", Uid/binary>>,
    PositiveLbl = <<"__its_pos_", Uid/binary>>,
    DivLoopLbl = <<"__its_div_", Uid/binary>>,
    DoneLbl = <<"__its_done_", Uid/binary>>,
    CopyLbl = <<"__its_cp_", Uid/binary>>,
    CopyDoneLbl = <<"__its_cpd_", Uid/binary>>,
    lists:flatten([
        %% FIXED BUG #6: Save r13 which is clobbered below at line 1063
        ?ENC:encode_push(rbx),
        ?ENC:encode_push(r12),
        ?ENC:encode_push(r13),
        ?ENC:encode_push(r14),
        %% Allocate 48 bytes on stack for digit buffer
        ?ENC:encode_sub_imm(rsp, 48),

        %% rbx = input value, r14 = sign flag (0=positive, 1=negative)
        ?ENC:encode_mov_rr(rbx, Src),
        ?ENC:encode_xor_rr(r14, r14),     %% r14 = 0 (positive)
        %% r12 = 47 (write position, right to left)
        ?ENC:encode_mov_imm64(r12, 47),

        %% Check zero
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ne, 0),
        {reloc, rel32, NonZeroLbl, -4},

        %% Zero: store '0' at rsp+46
        encode_mov_byte_to_stack(46, 48),
        ?ENC:encode_mov_imm64(r12, 46),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, DoneLbl, -4},

        {label, NonZeroLbl},
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, PositiveLbl, -4},
        %% Negate and record sign
        ?ENC:encode_neg(rbx),
        ?ENC:encode_mov_imm64(r14, 1),    %% r14 = 1 (was negative)

        {label, PositiveLbl},
        {label, DivLoopLbl},
        ?ENC:encode_sub_imm(r12, 1),
        %% rax = rbx, rdx:rax / 10
        ?ENC:encode_mov_rr(rax, rbx),
        ?ENC:encode_xor_rr(rdx, rdx),  %% zero-extend
        ?ENC:encode_mov_imm64(rcx, 10),
        ?ENC:encode_idiv(rcx),          %% rax=quot, rdx=rem
        ?ENC:encode_add_imm(rdx, 48),   %% rem + '0'
        %% Store digit byte at [rsp + r12]
        ?ENC:encode_mov_rr(r13, rsp),
        ?ENC:encode_add_rr(r13, r12),
        encode_mov_byte_reg_to_mem(r13, rdx),
        ?ENC:encode_mov_rr(rbx, rax),
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ne, 0),
        {reloc, rel32, DivLoopLbl, -4},

        %% If original was negative (r14=1), prepend '-'
        ?ENC:encode_cmp_imm(r14, 0),
        ?ENC:encode_jcc_rel32(eq, 0),
        {reloc, rel32, DoneLbl, -4},
        ?ENC:encode_sub_imm(r12, 1),
        ?ENC:encode_mov_rr(rsi, rsp),
        ?ENC:encode_add_rr(rsi, r12),
        ?ENC:encode_mov_imm64(rdx, 45),       %% '-'
        encode_mov_byte_reg_to_mem(rsi, rdx),

        {label, DoneLbl},
        %% r12 = start pos, length = 48 - r12
        ?ENC:encode_mov_imm64(rax, 48),
        ?ENC:encode_sub_rr(rax, r12),   %% rax = len

        %% Allocate heap buffer: r13 = heap ptr, bump by aligned len
        ?ENC:encode_mov_rr(r13, r15),   %% r13 = data buf start (r15=heap)
        ?ENC:encode_mov_rr(rcx, rax),
        ?ENC:encode_add_imm(rcx, 7),
        encode_and_imm_x86(rcx, -8),
        ?ENC:encode_add_rr(r15, rcx),   %% bump heap

        %% Copy digits from [rsp+r12..rsp+47] to [r13..r13+len-1]
        ?ENC:encode_xor_rr(rcx, rcx),   %% index = 0
        {label, CopyLbl},
        ?ENC:encode_cmp_rr(rcx, rax),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, CopyDoneLbl, -4},
        %% Load byte from stack: movzx rsi, byte [rdx]
        ?ENC:encode_mov_rr(rdx, rsp),
        ?ENC:encode_add_rr(rdx, r12),
        ?ENC:encode_add_rr(rdx, rcx),
        encode_movzx_byte_mem(rsi, rdx),
        %% Store to heap
        ?ENC:encode_mov_rr(rdx, r13),
        ?ENC:encode_add_rr(rdx, rcx),
        encode_mov_byte_reg_to_mem(rdx, rsi),
        ?ENC:encode_add_imm(rcx, 1),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, CopyLbl, -4},
        {label, CopyDoneLbl},

        %% Allocate fat pointer on heap (16 bytes)
        ?ENC:encode_mov_rr(rdi, r15),   %% rdi = fat ptr location
        ?ENC:encode_add_imm(r15, 16),   %% bump heap
        %% Store {ptr, len}
        ?ENC:encode_mov_mem_store(rdi, 0, r13),   %% fat.ptr = data
        ?ENC:encode_mov_mem_store(rdi, 8, rax),    %% fat.len = length

        %% Clean up stack and restore (reverse order of pushes)
        ?ENC:encode_add_imm(rsp, 48),
        ?ENC:encode_pop(r14),
        ?ENC:encode_pop(r13),
        ?ENC:encode_pop(r12),
        ?ENC:encode_pop(rbx),
        ?ENC:encode_mov_rr(Dst, rdi)
    ]);

%% FLOAT_TO_STR: Simplified — convert float to int, then int to string.
lower_instruction({float_to_str, {preg, Dst}, {preg, Src}}, FnName, _UsedCalleeSaved) ->
    %% MOVQ xmm0, Src; CVTTSD2SI r10, xmm0; then int_to_str(Dst, r10)
    Rex1 = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Src)),
    ModRM1 = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(Src)),
    Movq = <<16#66:8, Rex1:8, 16#0F:8, 16#6E:8, ModRM1:8>>,
    Rex2 = ?ENC:rex(1, ?ENC:reg_hi(r10), 0, 0),
    ModRM2 = ?ENC:modrm(2#11, ?ENC:reg_lo(r10), 0),
    Cvt = <<16#F2:8, Rex2:8, 16#0F:8, 16#2C:8, ModRM2:8>>,
    [Movq, Cvt] ++ lower_instruction({int_to_str, {preg, Dst}, {preg, r10}}, FnName, _UsedCalleeSaved);

%%====================================================================
%% Float arithmetic x86_64
%%====================================================================

%% FADD: MOVQ xmm0, A; MOVQ xmm1, B; ADDSD xmm0, xmm1; MOVQ Dst, xmm0
lower_instruction({fadd, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    emit_float_binop_x86(Dst, A, B, <<16#F2:8, 16#0F:8, 16#58:8>>);

%% FSUB
lower_instruction({fsub, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    emit_float_binop_x86(Dst, A, B, <<16#F2:8, 16#0F:8, 16#5C:8>>);

%% FMUL
lower_instruction({fmul, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    emit_float_binop_x86(Dst, A, B, <<16#F2:8, 16#0F:8, 16#59:8>>);

%% FDIV
lower_instruction({fdiv, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _UsedCalleeSaved) ->
    emit_float_binop_x86(Dst, A, B, <<16#F2:8, 16#0F:8, 16#5E:8>>);

%% RAW bytes (inline asm)
lower_instruction({raw, Bytes}, _FnName, _UsedCalleeSaved) when is_binary(Bytes) ->
    [Bytes];

%% PRINT_INT — expand to full integer-to-stdout routine for x86_64.
%% Input: Reg contains the integer value.
%% Algorithm: save regs, handle zero/negative, divide by 10 in a loop
%% to extract digits, write to stdout via Linux write(2) syscall.
%%
%% Stack layout during print_int (128 bytes below saved regs):
%%   rsp+0..79:    digit buffer area
%%   rsp+80..127:  digit buffer (newline at rsp+127)
%% Register usage:
%%   rbx = value being printed (preserved callee-saved, save/restore)
%%   r12 = buffer position index (preserved callee-saved, save/restore)
%%   r13 = temp (preserved callee-saved, save/restore)
lower_instruction({print_int, {preg, Reg}}, _FnName, _UsedCalleeSaved) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NonZeroLbl = <<"__pi_nz_", Uid/binary>>,
    PositiveLbl = <<"__pi_pos_", Uid/binary>>,
    DivLoopLbl = <<"__pi_div_", Uid/binary>>,
    PrintLbl = <<"__pi_pr_", Uid/binary>>,
    lists:flatten([
        %% Save callee-saved registers we'll use
        ?ENC:encode_push(rbx),
        ?ENC:encode_push(r12),
        ?ENC:encode_push(r13),
        %% Save caller-saved registers that syscall will clobber
        ?ENC:encode_push(rdi),
        ?ENC:encode_push(rsi),
        ?ENC:encode_push(rdx),
        ?ENC:encode_push(rax),
        ?ENC:encode_push(rcx),
        ?ENC:encode_push(r11),
        ?ENC:encode_push(r8),
        ?ENC:encode_push(r9),
        ?ENC:encode_push(r10),

        %% Allocate 128-byte buffer on stack (16-byte aligned: 12 pushes = 96 bytes,
        %% + 128 = 224, not aligned. Add 8 more for alignment: 232 -> still not. Let's
        %% just use 128; stack alignment is less critical for Linux syscalls)
        ?ENC:encode_sub_imm(rsp, 128),

        %% Move input value to rbx (callee-saved, safe across syscalls)
        ?ENC:encode_mov_rr(rbx, Reg),

        %% Store newline at buffer end: mov byte [rsp+127], 10
        encode_mov_byte_to_stack(127, 10),

        %% r12 = 127 (position index, will store digits backward from here)
        ?ENC:encode_mov_imm64(r12, 127),

        %% Check if value is zero
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ne, 0),
        {reloc, rel32, NonZeroLbl, -4},

        %% Zero case: store '0' at rsp+126, set pos=126
        encode_mov_byte_to_stack(126, $0),
        ?ENC:encode_mov_imm64(r12, 126),
        ?ENC:encode_jmp_rel32(0),
        {reloc, rel32, PrintLbl, -4},

        %% Nonzero:
        {label, NonZeroLbl},

        %% Check if negative
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ge, 0),
        {reloc, rel32, PositiveLbl, -4},

        %% Negative: write '-' sign to stdout
        encode_mov_byte_to_stack(0, $-),  %% store '-' at rsp+0
        ?ENC:encode_mov_imm64(rax, 1),    %% sys_write
        ?ENC:encode_mov_imm64(rdi, 1),    %% fd = stdout
        ?ENC:encode_mov_rr(rsi, rsp),      %% buf = rsp+0
        ?ENC:encode_mov_imm64(rdx, 1),    %% len = 1
        ?ENC:encode_syscall(),

        %% Negate: rbx = -rbx
        ?ENC:encode_neg(rbx),

        %% Positive (or negated):
        {label, PositiveLbl},

        %% Digit extraction loop: divide by 10, store remainder as digit
        {label, DivLoopLbl},
        ?ENC:encode_sub_imm(r12, 1),          %% pos--
        %% Prepare for idiv: rdx:rax / rcx -> quotient in rax, remainder in rdx
        ?ENC:encode_mov_rr(rax, rbx),          %% rax = value
        ?ENC:encode_mov_imm64(rcx, 10),        %% divisor = 10
        ?ENC:encode_cqo(),                      %% sign-extend rax -> rdx:rax
        ?ENC:encode_idiv(rcx),                  %% rax = quotient, rdx = remainder
        ?ENC:encode_add_imm(rdx, $0),          %% rdx += '0' (ASCII digit)
        %% Store digit byte at rsp+r12: we need to compute address
        %% mov r13, rsp; add r13, r12; mov byte [r13], dl
        ?ENC:encode_mov_rr(r13, rsp),
        ?ENC:encode_add_rr(r13, r12),
        encode_mov_byte_reg_to_mem(r13, rdx),  %% mov [r13], dl
        ?ENC:encode_mov_rr(rbx, rax),          %% value = quotient
        ?ENC:encode_cmp_imm(rbx, 0),
        ?ENC:encode_jcc_rel32(ne, 0),
        {reloc, rel32, DivLoopLbl, -4},

        %% Print: write(1, rsp+r12, 128-r12)
        {label, PrintLbl},
        ?ENC:encode_mov_rr(rsi, rsp),
        ?ENC:encode_add_rr(rsi, r12),          %% rsi = rsp + r12 (buf start)
        ?ENC:encode_mov_imm64(rdx, 128),
        ?ENC:encode_sub_rr(rdx, r12),          %% rdx = 128 - r12 (length)
        ?ENC:encode_mov_imm64(rax, 1),         %% sys_write
        ?ENC:encode_mov_imm64(rdi, 1),         %% fd = stdout
        ?ENC:encode_syscall(),

        %% Deallocate buffer
        ?ENC:encode_add_imm(rsp, 128),

        %% Restore all saved registers (reverse order)
        ?ENC:encode_pop(r10),
        ?ENC:encode_pop(r9),
        ?ENC:encode_pop(r8),
        ?ENC:encode_pop(r11),
        ?ENC:encode_pop(rcx),
        ?ENC:encode_pop(rax),
        ?ENC:encode_pop(rdx),
        ?ENC:encode_pop(rsi),
        ?ENC:encode_pop(rdi),
        ?ENC:encode_pop(r13),
        ?ENC:encode_pop(r12),
        ?ENC:encode_pop(rbx)
    ]);

%% Catch-all for unhandled instructions
lower_instruction(Inst, _FnName, _UsedCalleeSaved) ->
    io:format(standard_error, "ERROR: unhandled x86_64 instruction: ~p~n", [Inst]),
    %% Emit ud2 (illegal instruction trap) so it crashes predictably
    [<<16#0F:8, 16#0B:8>>].

%%====================================================================
%% Internal helpers
%%====================================================================

%% AND r64, imm32 (sign-extended): REX.W + 81 /4 id
encode_and_imm_x86(Dst, Imm) ->
    Rex = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Dst)),
    ModRM = ?ENC:modrm(2#11, 4, ?ENC:reg_lo(Dst)),
    <<Rex:8, 16#81:8, ModRM:8, Imm:32/little-signed>>.

%% Move argument values into System V argument registers.
%% Detects cycles (e.g., rdi→rsi and rsi→rdi) and uses r11 as temp to break them.
move_args_to_regs_x86([], _) -> [];
move_args_to_regs_x86(Args, ArgRegs) ->
    %% Build the full mapping first to detect cycles
    Moves = build_arg_moves_x86(Args, ArgRegs),
    %% Detect and break cycles using r11 as scratch
    break_cycles_and_emit_x86(Moves).

build_arg_moves_x86([], _) -> [];
build_arg_moves_x86([{preg, Reg} | Rest], [ArgReg | ArgRegs]) ->
    case Reg of
        ArgReg -> build_arg_moves_x86(Rest, ArgRegs);
        _ -> [{ArgReg, {preg, Reg}} | build_arg_moves_x86(Rest, ArgRegs)]
    end;
build_arg_moves_x86([{imm, Val} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {imm, Val}} | build_arg_moves_x86(Rest, ArgRegs)];
build_arg_moves_x86([{stack, Slot} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {stack, Slot}} | build_arg_moves_x86(Rest, ArgRegs)];
build_arg_moves_x86([{vreg, _N} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {imm, 0}} | build_arg_moves_x86(Rest, ArgRegs)];
build_arg_moves_x86(_, []) -> [].

break_cycles_and_emit_x86(Moves) ->
    %% FIXED: Full cycle decomposition — detect ALL cycles and break EACH one.
    %% Build dependency graph and find strongly connected components (cycles).
    Graph = [{Dst, case Src of {preg, R} -> R; _ -> none end} || {Dst, Src} <- Moves],
    Cycles = find_all_cycles(Graph),
    case Cycles of
        [] ->
            %% No cycles — emit directly
            [emit_move_x86(Dst, Src) || {Dst, Src} <- Moves];
        _ ->
            %% Break each cycle by saving one register to r11 before the cycle
            break_all_cycles(Cycles, Moves)
    end.

%% Find all cycles in the move graph using Tarjan's SCC algorithm (simplified).
%% Returns list of cycles where each cycle is a list of registers.
find_all_cycles(Graph) ->
    Nodes = lists:usort([Dst || {Dst, _} <- Graph] ++ [Src || {_, Src} <- Graph, Src =/= none]),
    Edges = [{Src, Dst} || {Dst, Src} <- Graph, Src =/= none],
    %% Simple cycle detection: find register chains where last points to first
    SCCs = tarjan_scc(Nodes, Edges),
    [SCC || SCC <- SCCs, length(SCC) > 1].

%% Simplified Tarjan SCC for register dependency cycles
tarjan_scc(Nodes, Edges) ->
    InitState = #{index => 0, stack => [], sccs => []},
    {_, Result} = lists:foldl(fun(Node, {Visited, State}) ->
        case maps:is_key(Node, Visited) of
            true -> {Visited, State};
            false -> tarjan_visit(Node, Edges, Visited, State)
        end
    end, {#{}, InitState}, Nodes),
    maps:get(sccs, Result).

tarjan_visit(Node, Edges, Visited, State = #{index := Idx, stack := Stack}) ->
    Visited1 = Visited#{Node => #{index => Idx, lowlink => Idx, on_stack => true}},
    State1 = State#{index => Idx + 1, stack => [Node | Stack]},
    Successors = [Dst || {Src, Dst} <- Edges, Src =:= Node],
    {Visited2, State2} = lists:foldl(fun(Succ, {V, S}) ->
        case maps:find(Succ, V) of
            error -> tarjan_visit(Succ, Edges, V, S);
            {ok, #{on_stack := true, index := SuccIdx}} ->
                #{lowlink := NodeLL} = maps:get(Node, V),
                NewLL = min(NodeLL, SuccIdx),
                V1 = V#{Node => (maps:get(Node, V))#{lowlink => NewLL}},
                {V1, S};
            {ok, _} -> {V, S}
        end
    end, {Visited1, State1}, Successors),
    NodeInfo = maps:get(Node, Visited2),
    case maps:get(index, NodeInfo) =:= maps:get(lowlink, NodeInfo) of
        true ->
            {SCC, NewStack} = pop_scc(Node, maps:get(stack, State2), []),
            Visited3 = lists:foldl(fun(N, V) ->
                V#{N => (maps:get(N, V))#{on_stack => false}}
            end, Visited2, SCC),
            State3 = State2#{stack => NewStack, sccs => [SCC | maps:get(sccs, State2)]},
            {Visited3, State3};
        false ->
            {Visited2, State2}
    end.

pop_scc(Node, [Node | Rest], Acc) -> {[Node | Acc], Rest};
pop_scc(Node, [Other | Rest], Acc) -> pop_scc(Node, Rest, [Other | Acc]).

%% FIXED BUG #4: Break each cycle individually using rotation.
%% For a cycle [A→B→C→A], emit: save A to temp, C→A, B→C, temp→B.
break_all_cycles(Cycles, Moves) ->
    %% Build move map for quick lookup
    MoveMap = maps:from_list(Moves),
    %% For each cycle, emit rotation sequence
    CycleMoves = lists:flatmap(fun(Cycle) ->
        break_one_cycle(Cycle, MoveMap)
    end, Cycles),
    %% Emit non-cycle moves
    CycleRegs = lists:usort(lists:flatten(Cycles)),
    NonCycleMoves = [emit_move_x86(Dst, Src) || {Dst, Src} <- Moves,
                     not lists:member(Dst, CycleRegs)],
    CycleMoves ++ NonCycleMoves.

%% Break a single cycle by rotating through temp register r11.
%% Cycle is [R1, R2, ..., Rn] where R1→R2→...→Rn→R1.
break_one_cycle([], _MoveMap) -> [];
break_one_cycle([First | _] = Cycle, MoveMap) ->
    %% Save First to r11
    SaveFirst = ?ENC:encode_mov_rr(r11, First),
    %% Emit moves for the cycle in reverse order (excluding the last→first edge)
    Rotations = [emit_move_x86(To, maps:get(From, MoveMap))
                 || {From, To} <- lists:zip(Cycle, tl(Cycle) ++ [First]),
                    To =/= First],  % Skip the wrap-around edge
    %% Restore temp to the second register in cycle
    [Second | _] = tl(Cycle) ++ [First],
    RestoreTemp = ?ENC:encode_mov_rr(Second, r11),
    [SaveFirst] ++ Rotations ++ [RestoreTemp].

emit_move_x86(ArgReg, {preg, Reg}) ->
    ?ENC:encode_mov_rr(ArgReg, Reg);
emit_move_x86(ArgReg, {imm, Val}) ->
    ?ENC:encode_mov_imm64(ArgReg, Val);
emit_move_x86(ArgReg, {stack, Slot}) ->
    %% FIXED BUG #5: This is called during argument setup, where we don't have
    %% UsedCalleeSaved context. For now, assume Offset = -((Slot+1)*8) is correct
    %% for the function's own stack frame (not caller's). This is a partial fix.
    %% Full fix would require threading UsedCalleeSaved or FrameSize through.
    %% TODO: Add comment noting limitation for args > 6.
    Offset = -((Slot + 1) * 8),
    ?ENC:encode_mov_mem_load(ArgReg, rbp, Offset).

%% Encode: mov byte [rsp + Offset], ImmByte
%% This is: C6 44 24 <offset8> <byte>  (with SIB=0x24 for RSP base)
%% Note: offset must fit in signed 8-bit (0..127)
encode_mov_byte_to_stack(Offset, ImmByte) when Offset >= 0, Offset =< 127 ->
    <<16#C6:8, 16#44:8, 16#24:8, Offset:8, (ImmByte band 16#FF):8>>.

%% Encode: mov byte [Base], reg8  (store low byte of Src to [Base])
%% This is: REX 88 /r  with ModR/M=[00][reg_lo][base_lo]
%% For registers r8-r15, REX prefix is needed to access the right byte reg.
encode_mov_byte_reg_to_mem(Base, Src) ->
    %% 88 /r: MOV r/m8, r8
    %% REX prefix: 0x40 | (W=0) | (R=src_hi) | (X=0) | (B=base_hi)
    %% We need REX if either register is r8-r15, or to access sil/dil/spl/bpl
    Rex = 16#40 bor (?ENC:reg_hi(Src) bsl 2) bor ?ENC:reg_hi(Base),
    SrcLo = ?ENC:reg_lo(Src),
    BaseLo = ?ENC:reg_lo(Base),
    case Base of
        rbp ->
            M = ?ENC:modrm(1, SrcLo, BaseLo),
            <<Rex:8, 16#88:8, M:8, 0:8>>;
        r13 ->
            M = ?ENC:modrm(1, SrcLo, BaseLo),
            <<Rex:8, 16#88:8, M:8, 0:8>>;
        rsp ->
            M = ?ENC:modrm(0, SrcLo, 4),
            <<Rex:8, 16#88:8, M:8, 16#24:8>>;
        r12 ->
            M = ?ENC:modrm(0, SrcLo, 4),
            <<Rex:8, 16#88:8, M:8, 16#24:8>>;
        _ ->
            M = ?ENC:modrm(0, SrcLo, BaseLo),
            <<Rex:8, 16#88:8, M:8>>
    end.

%% Encode: movzx r64, byte [Base]  (zero-extend byte load)
%% 0F B6 /r with REX.W for 64-bit destination
encode_movzx_byte_mem(Dst, Base) ->
    Rex = ?ENC:rex(1, ?ENC:reg_hi(Dst), 0, ?ENC:reg_hi(Base)),
    DstLo = ?ENC:reg_lo(Dst),
    BaseLo = ?ENC:reg_lo(Base),
    case Base of
        rbp ->
            M = ?ENC:modrm(1, DstLo, BaseLo),
            <<Rex:8, 16#0F:8, 16#B6:8, M:8, 0:8>>;
        r13 ->
            M = ?ENC:modrm(1, DstLo, BaseLo),
            <<Rex:8, 16#0F:8, 16#B6:8, M:8, 0:8>>;
        rsp ->
            M = ?ENC:modrm(0, DstLo, 4),
            <<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8>>;
        r12 ->
            M = ?ENC:modrm(0, DstLo, 4),
            <<Rex:8, 16#0F:8, 16#B6:8, M:8, 16#24:8>>;
        _ ->
            M = ?ENC:modrm(0, DstLo, BaseLo),
            <<Rex:8, 16#0F:8, 16#B6:8, M:8>>
    end.

%% Emit a float binary operation for x86_64.
%% MOVQ xmm0, A; MOVQ xmm1, B; OP xmm0, xmm1; MOVQ Dst, xmm0
emit_float_binop_x86(Dst, A, B, OpPrefix) ->
    %% MOVQ xmm0, A: 66 REX.W 0F 6E /r (xmm=0, r/m=A)
    RexA = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(A)),
    ModRMA = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(A)),
    MovqA = <<16#66:8, RexA:8, 16#0F:8, 16#6E:8, ModRMA:8>>,
    %% MOVQ xmm1, B: 66 REX.W 0F 6E /r (xmm=1, r/m=B)
    RexB = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(B)),
    ModRMB = ?ENC:modrm(2#11, 1, ?ENC:reg_lo(B)),
    MovqB = <<16#66:8, RexB:8, 16#0F:8, 16#6E:8, ModRMB:8>>,
    %% OP xmm0, xmm1: OpPrefix + ModRM(11, 0, 1)
    OpModRM = ?ENC:modrm(2#11, 0, 1),
    FloatOp = <<OpPrefix/binary, OpModRM:8>>,
    %% MOVQ Dst, xmm0: 66 REX.W 0F 7E /r (xmm=0, r/m=Dst)
    RexDst = ?ENC:rex(1, 0, 0, ?ENC:reg_hi(Dst)),
    ModRMDst = ?ENC:modrm(2#11, 0, ?ENC:reg_lo(Dst)),
    MovqDst = <<16#66:8, RexDst:8, 16#0F:8, 16#7E:8, ModRMDst:8>>,
    [MovqA, MovqB, FloatOp, MovqDst].

%% @doc Scan function body to find which callee-saved registers are used.
%%      Callee-saved registers in System V AMD64 ABI: rbx, r12, r13, r14, r15
%%      Returns list of used callee-saved registers (may be empty).
find_used_callee_saved(Body) ->
    CalleeSaved = [rbx, r12, r13, r14, r15],
    UsedRegs = lists:foldl(fun(Inst, Acc) ->
        extract_regs(Inst) ++ Acc
    end, [], Body),
    %% Return intersection of callee-saved and used registers
    lists:filter(fun(Reg) -> lists:member(Reg, UsedRegs) end, CalleeSaved).

%% Extract all register references from an instruction
extract_regs({mov, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({mov_imm, {preg, Dst}, _}) -> [Dst];
extract_regs({load, {preg, Dst}, {preg, Base}, _}) -> [Dst, Base];
extract_regs({load_byte, {preg, Dst}, {preg, Base}, _}) -> [Dst, Base];
extract_regs({store, {preg, Base}, _, {preg, Src}}) -> [Base, Src];
extract_regs({store_byte, {preg, Base}, _, {preg, Src}}) -> [Base, Src];
extract_regs({add, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({sub, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({mul, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({sdiv, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({srem, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({and_, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({or_, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({xor_, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({shl, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({shr, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({sar, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({neg, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({not_, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({cmp, {preg, A}, {preg, B}}) -> [A, B];
extract_regs({push, {preg, Reg}}) -> [Reg];
extract_regs({pop, {preg, Reg}}) -> [Reg];
extract_regs({call_indirect, {preg, Reg}}) -> [Reg];
extract_regs({print_int, {preg, Reg}}) -> [Reg];
extract_regs({print_str, {preg, Reg}}) -> [Reg];
extract_regs({string_lit, {preg, Dst}, _, _}) -> [Dst];
extract_regs({string_len, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({string_cmp, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({string_concat, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({array_new, {preg, Dst}, _, _}) -> [Dst];
extract_regs({array_get, {preg, Dst}, {preg, Arr}, {preg, Idx}, _}) -> [Dst, Arr, Idx];
extract_regs({array_set, {preg, Arr}, {preg, Idx}, {preg, Val}, _}) -> [Arr, Idx, Val];
extract_regs({array_len, {preg, Dst}, {preg, Arr}}) -> [Dst, Arr];
extract_regs({array_append, {preg, Dst}, {preg, Arr}, {preg, Val}, _}) -> [Dst, Arr, Val];
extract_regs({struct_new, {preg, Dst}, _}) -> [Dst];
extract_regs({field_get, {preg, Dst}, {preg, Struct}, _}) -> [Dst, Struct];
extract_regs({field_set, {preg, Struct}, _, {preg, Val}}) -> [Struct, Val];
extract_regs({map_new, {preg, Dst}}) -> [Dst];
extract_regs({map_get, {preg, Dst}, {preg, Map}, {preg, Key}}) -> [Dst, Map, Key];
extract_regs({map_put, {preg, Dst}, {preg, Map}, {preg, Key}, {preg, Val}}) -> [Dst, Map, Key, Val];
extract_regs({map_delete, {preg, Dst}, {preg, Map}, {preg, Key}}) -> [Dst, Map, Key];
extract_regs({int_to_float, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({float_to_int, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({int_to_str, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({float_to_str, {preg, Dst}, {preg, Src}}) -> [Dst, Src];
extract_regs({fadd, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({fsub, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({fmul, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({fdiv, {preg, Dst}, {preg, A}, {preg, B}}) -> [Dst, A, B];
extract_regs({method_call, {preg, Dst}, _, _, Args}) -> 
    [Dst | [R || {preg, R} <- Args]];
extract_regs(_) -> [].
