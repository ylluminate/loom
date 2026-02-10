-module(vbeam_native_lower_arm64).

%% Lower IR instructions to ARM64 machine code.
%% Produces code parts (binaries + label/reloc markers).

-export([
    lower_function/2
]).

-define(ENC, vbeam_native_arm64).

%% Lower a complete function.
-spec lower_function(map(), term()) -> {[term()], term()}.
lower_function(#{name := Name, body := Body, arity := _Arity,
                 spill_slots := SpillSlots} = Fn, LinkState) ->
    UsedCallee = maps:get(used_callee_saved, Fn, []),
    FrameSize = compute_frame_size(SpillSlots, UsedCallee),
    Format = case maps:find(ctx, Fn) of
        {ok, #{format := F}} -> F;
        _ -> elf64  %% default to Linux convention
    end,
    Prologue = emit_prologue(FrameSize, UsedCallee),
    BodyParts = lower_body(Body, Name, FrameSize, Format, UsedCallee),
    Parts = Prologue ++ BodyParts,
    {Parts, LinkState}.

%% Stack frame size: 16-byte aligned, includes space for:
%%   - x29+x30 save (16 bytes)
%%   - callee-saved registers used by the function (8 bytes each)
%%   - spill slots (8 bytes each)
compute_frame_size(SpillSlots, UsedCallee) ->
    NumCallee = length(UsedCallee),
    RawSize = 16 + NumCallee * 8 + SpillSlots * 8,
    case RawSize rem 16 of
        0 -> RawSize;
        R -> RawSize + (16 - R)
    end.

%% Prologue: stp x29, x30, [sp, #-FrameSize]!; mov x29, sp; save callee-saved
%% For large frames (>504 bytes), split into stp + sub
emit_prologue(FrameSize, UsedCallee) when FrameSize =< 504 ->
    [?ENC:encode_stp_pre(x29, x30, sp, -FrameSize),
     ?ENC:encode_mov_rr(x29, sp)] ++
    save_callee_saved(UsedCallee, 16);
emit_prologue(FrameSize, UsedCallee) ->
    %% Large frame: stp x29,x30,[sp,#-16]! then sub sp,sp,#(FrameSize-16)
    %% CRITICAL: Set x29 AFTER full stack allocation, not before
    %% If (FrameSize-16) > 4095, materialize in x16 and use register form
    FrameDelta = FrameSize - 16,
    SubInstr = case FrameDelta =< 4095 of
        true ->
            [?ENC:encode_sub_imm(sp, sp, FrameDelta)];
        false ->
            [?ENC:encode_mov_imm64(x16, FrameDelta),
             ?ENC:encode_sub_rrr(sp, sp, x16)]
    end,
    [?ENC:encode_stp_pre(x29, x30, sp, -16)] ++
    SubInstr ++
    [?ENC:encode_add_imm(x29, sp, 0)] ++
    save_callee_saved(UsedCallee, 16).

%% Epilogue: restore callee-saved; mov sp, x29; ldp x29, x30, [sp], #FrameSize; ret
%% For large frames (>504 bytes), split into add + ldp
emit_epilogue(FrameSize, UsedCallee) when FrameSize =< 504 ->
    restore_callee_saved(UsedCallee, 16) ++
    [?ENC:encode_mov_rr(sp, x29),
     ?ENC:encode_ldp_post(x29, x30, sp, FrameSize),
     ?ENC:encode_ret()];
emit_epilogue(FrameSize, UsedCallee) ->
    %% Large frame: add sp,sp,#(FrameSize-16) then ldp x29,x30,[sp],#16
    %% If (FrameSize-16) > 4095, materialize in x16 and use register form
    FrameDelta = FrameSize - 16,
    AddInstr = case FrameDelta =< 4095 of
        true ->
            [?ENC:encode_add_imm(sp, sp, FrameDelta)];
        false ->
            [?ENC:encode_mov_imm64(x16, FrameDelta),
             ?ENC:encode_add_rrr(sp, sp, x16)]
    end,
    restore_callee_saved(UsedCallee, 16) ++
    [?ENC:encode_mov_rr(sp, x29)] ++
    AddInstr ++
    [?ENC:encode_ldp_post(x29, x30, sp, 16),
     ?ENC:encode_ret()].

%% Save callee-saved registers to stack frame.
%% Stores at [x29, #offset], [x29, #offset+8], etc.
save_callee_saved([], _Offset) -> [];
save_callee_saved([Reg | Rest], Offset) ->
    [?ENC:encode_str(Reg, x29, Offset) | save_callee_saved(Rest, Offset + 8)].

%% Restore callee-saved registers from stack frame.
restore_callee_saved([], _Offset) -> [];
restore_callee_saved([Reg | Rest], Offset) ->
    [?ENC:encode_ldr(Reg, x29, Offset) | restore_callee_saved(Rest, Offset + 8)].

%% Lower body instructions.
lower_body([], _FnName, _FS, _Fmt, _UC) -> [];
lower_body([Inst | Rest], FnName, FS, Fmt, UC) ->
    Parts = lower_instruction(Inst, FnName, FS, Fmt, UC),
    Parts ++ lower_body(Rest, FnName, FS, Fmt, UC).

%% Labels
lower_instruction({label, Name}, _FnName, _FS, _Fmt, _UC) ->
    [{label, Name}];

lower_instruction({comment, _}, _FnName, _FS, _Fmt, _UC) -> [];
lower_instruction(nop, _FnName, _FS, _Fmt, _UC) -> [?ENC:encode_nop()];

%% MOV register to register
lower_instruction({mov, {preg, Dst}, {preg, Src}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mov_rr(Dst, Src)];

%% MOV immediate
lower_instruction({mov_imm, {preg, Dst}, Imm}, _FnName, _FS, _Fmt, _UC) when is_integer(Imm) ->
    [?ENC:encode_mov_imm64(Dst, Imm)];

%% MOV with stack slots
lower_instruction({mov, {stack, Slot}, {preg, Src}}, _FnName, _FS, _Fmt, UC) ->
    %% Store to [x29 + offset]
    %% Offset must account for callee-saved registers after x29+x30
    NumCalleeSaved = length(UC),
    Offset = 16 + NumCalleeSaved * 8 + Slot * 8,
    [?ENC:encode_str(Src, x29, Offset)];
lower_instruction({mov, {preg, Dst}, {stack, Slot}}, _FnName, _FS, _Fmt, UC) ->
    %% Load from [x29 + offset]
    %% Offset must account for callee-saved registers after x29+x30
    NumCalleeSaved = length(UC),
    Offset = 16 + NumCalleeSaved * 8 + Slot * 8,
    [?ENC:encode_ldr(Dst, x29, Offset)];

%% LOAD from memory
lower_instruction({load, {preg, Dst}, {preg, Base}, Off}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_ldr(Dst, Base, Off)];

%% LOAD_BYTE from memory (zero-extends byte to 64-bit)
lower_instruction({load_byte, {preg, Dst}, {preg, Base}, Off}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_ldrb(Dst, Base, Off)];

%% STORE to memory
lower_instruction({store, {preg, Base}, Off, {preg, Src}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_str(Src, Base, Off)];

%% STORE_BYTE to memory
lower_instruction({store_byte, {preg, Base}, Off, {preg, Src}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_strb(Src, Base, Off)];

%% ADD
lower_instruction({add, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_add_rrr(Dst, A, B)];
lower_instruction({add, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC)
    when Imm >= 0, Imm < 4096 ->
    [?ENC:encode_add_imm(Dst, A, Imm)];
lower_instruction({add, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    %% Large immediate: load to temp register first
    [?ENC:encode_mov_imm64(x16, Imm),
     ?ENC:encode_add_rrr(Dst, A, x16)];

%% SUB
lower_instruction({sub, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_sub_rrr(Dst, A, B)];
lower_instruction({sub, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC)
    when Imm >= 0, Imm < 4096 ->
    [?ENC:encode_sub_imm(Dst, A, Imm)];
lower_instruction({sub, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mov_imm64(x16, Imm),
     ?ENC:encode_sub_rrr(Dst, A, x16)];

%% MUL
lower_instruction({mul, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mul(Dst, A, B)];
lower_instruction({mul, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mov_imm64(x16, Imm),
     ?ENC:encode_mul(Dst, A, x16)];

%% SDIV
lower_instruction({sdiv, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_sdiv(Dst, A, B)];
lower_instruction({sdiv, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mov_imm64(x16, Imm),
     ?ENC:encode_sdiv(Dst, A, x16)];

%% SREM (remainder = A - (A/B)*B, using MSUB)
lower_instruction({srem, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    %% Use x16 as temp for quotient
    [?ENC:encode_sdiv(x16, A, B),
     ?ENC:encode_msub(Dst, x16, B, A)];
lower_instruction({srem, {preg, Dst}, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    %% Load imm to x17, use x16 for quotient
    [?ENC:encode_mov_imm64(x17, Imm),
     ?ENC:encode_sdiv(x16, A, x17),
     ?ENC:encode_msub(Dst, x16, x17, A)];

%% AND
lower_instruction({and_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_and_rrr(Dst, A, B)];

%% OR
lower_instruction({or_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_orr_rrr(Dst, A, B)];

%% XOR
lower_instruction({xor_, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_eor_rrr(Dst, A, B)];

%% SHL
lower_instruction({shl, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_lsl_rrr(Dst, A, B)];

%% SHR (logical)
lower_instruction({shr, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_lsr_rrr(Dst, A, B)];

%% SAR (arithmetic)
lower_instruction({sar, {preg, Dst}, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_asr_rrr(Dst, A, B)];

%% NEG
lower_instruction({neg, {preg, Dst}, {preg, Src}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_neg(Dst, Src)];

%% NOT
lower_instruction({not_, {preg, Dst}, {preg, Src}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mvn(Dst, Src)];

%% CMP
lower_instruction({cmp, {preg, A}, {preg, B}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_cmp_rr(A, B)];
lower_instruction({cmp, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC)
    when Imm >= 0, Imm < 4096 ->
    [?ENC:encode_cmp_imm(A, Imm)];
lower_instruction({cmp, {preg, A}, {imm, Imm}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_mov_imm64(x16, Imm),
     ?ENC:encode_cmp_rr(A, x16)];

%% JMP (unconditional branch)
lower_instruction({jmp, Label}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_b(0),  %% placeholder
     {reloc, arm64_branch26, Label, -4}];

%% JCC (conditional branch)
lower_instruction({jcc, Cond, Label}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_b_cond(Cond, 0),  %% placeholder
     {reloc, arm64_cond_branch19, Label, -4}];

%% CALL by symbol
lower_instruction({call, {sym, Name}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_bl(0),  %% placeholder
     {reloc, arm64_branch26, Name, -4}];

%% CALL indirect
lower_instruction({call_indirect, {preg, Reg}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_blr(Reg)];

%% RET
lower_instruction(ret, _FnName, FS, _Fmt, UC) ->
    emit_epilogue(FS, UC);

%% PUSH (ARM64 doesn't have single push — use sub sp then str)
lower_instruction({push, {preg, Reg}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_sub_imm(sp, sp, 16),
     ?ENC:encode_str(Reg, sp, 0)];

%% POP
lower_instruction({pop, {preg, Reg}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_ldr(Reg, sp, 0),
     ?ENC:encode_add_imm(sp, sp, 16)];

%% LEA (load address of data symbol — use ADRP + ADD)
lower_instruction({lea, {preg, Dst}, {data_ref, Name}}, _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_adrp(Dst, 0),          %% 4 bytes: ADRP with placeholder
     {reloc, adrp_page21, Name, -4},    %% patch the ADRP instruction
     ?ENC:encode_add_imm(Dst, Dst, 0),  %% 4 bytes: ADD with placeholder offset
     {reloc, add_pageoff12, Name, -4}]; %% patch the ADD instruction

%% SYSCALL: SVC #0x80 on macOS, SVC #0 on Linux
lower_instruction(syscall, _FnName, _FS, Fmt, _UC) ->
    SvcImm = case Fmt of
        macho -> 16#80;
        _     -> 0  %% Linux, UEFI
    end,
    [?ENC:encode_svc(SvcImm)];

%% PRINT_INT — expand to full integer-to-stdout routine.
%% Input: Reg contains the integer value.
%% All caller-visible registers are preserved: this pseudo-instruction saves
%% and restores x0-x2, x9-x12, x16/x8, x19, x20 so that surrounding code
%% can keep live values in any register across a print_int call.
%%
%% Stack layout (128 bytes, 16-byte aligned):
%%   sp+ 0..15 : saved x19, x20  (STP in prologue allocation)
%%   sp+16..23 : saved x0
%%   sp+24..31 : saved x1
%%   sp+32..39 : saved x2
%%   sp+40..47 : saved x9
%%   sp+48..55 : saved x10
%%   sp+56..63 : saved x11
%%   sp+64..71 : saved x12
%%   sp+72..79 : saved SysNumReg (x16 or x8)
%%   sp+80..127: digit buffer (48 bytes, newline at sp+127)
lower_instruction({print_int, {preg, Reg}}, _FnName, _FS, Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NonZeroLbl = <<"__pi_nz_", Uid/binary>>,
    PositiveLbl = <<"__pi_pos_", Uid/binary>>,
    DivLoopLbl = <<"__pi_div_", Uid/binary>>,
    PrintLbl = <<"__pi_pr_", Uid/binary>>,
    _DoneLbl = <<"__pi_done_", Uid/binary>>,
    {SysNumReg, WriteNum, SvcImm} = case Fmt of
        macho -> {x16, 4, 16#80};
        _     -> {x8, 64, 0}
    end,
    lists:flatten([
        %% Save x19, x20 and allocate 128-byte frame
        ?ENC:encode_stp_pre(x19, x20, sp, -128),

        %% Save all other registers we will clobber
        ?ENC:encode_str(x0, sp, 16),
        ?ENC:encode_str(x1, sp, 24),
        ?ENC:encode_str(x2, sp, 32),
        ?ENC:encode_str(x9, sp, 40),
        ?ENC:encode_str(x10, sp, 48),
        ?ENC:encode_str(x11, sp, 56),
        ?ENC:encode_str(x12, sp, 64),
        ?ENC:encode_str(SysNumReg, sp, 72),

        %% Move input to x19 (after saves, in case Reg is one of x0-x12)
        ?ENC:encode_mov_rr(x19, Reg),

        %% Store newline at buf end (sp+127)
        ?ENC:encode_mov_imm64(x9, 10),    %% '\n'
        ?ENC:encode_strb(x9, sp, 127),

        %% x20 = 127 (position, will be decremented before first digit)
        ?ENC:encode_mov_imm64(x20, 127),

        %% Check zero
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ne, 0),
        {reloc, arm64_cond_branch19, NonZeroLbl, -4},

        %% Zero case: store '0' at sp+126
        ?ENC:encode_mov_imm64(x9, 48),    %% '0'
        ?ENC:encode_strb(x9, sp, 126),
        ?ENC:encode_mov_imm64(x20, 126),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, PrintLbl, -4},

        %% Nonzero
        {label, NonZeroLbl},

        %% Check negative
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, PositiveLbl, -4},

        %% Negative: print '-' sign
        ?ENC:encode_mov_imm64(x9, 45),    %% '-'
        ?ENC:encode_strb(x9, sp, 80),     %% temp at sp+80 (start of buffer)
        ?ENC:encode_mov_imm64(x0, 1),     %% fd = stdout
        ?ENC:encode_add_imm(x1, sp, 80),  %% buf = sp+80
        ?ENC:encode_mov_imm64(x2, 1),     %% len = 1
        ?ENC:encode_mov_imm64(SysNumReg, WriteNum),
        ?ENC:encode_svc(SvcImm),

        %% Negate value
        ?ENC:encode_neg(x19, x19),

        %% Positive (or negated)
        {label, PositiveLbl},

        %% Digit extraction loop
        {label, DivLoopLbl},
        ?ENC:encode_sub_imm(x20, x20, 1),     %% pos--
        ?ENC:encode_mov_imm64(x9, 10),
        ?ENC:encode_sdiv(x10, x19, x9),       %% x10 = x19 / 10
        ?ENC:encode_msub(x11, x10, x9, x19),  %% x11 = x19 % 10
        ?ENC:encode_add_imm(x11, x11, 48),    %% x11 += '0'
        %% Store digit: strb x11, [sp + x20]
        %% Compute addr = sp + x20
        ?ENC:encode_add_imm(x12, sp, 0),      %% x12 = sp
        ?ENC:encode_add_rrr(x12, x12, x20),   %% x12 = sp + x20
        ?ENC:encode_strb(x11, x12, 0),        %% strb digit, [x12]
        ?ENC:encode_mov_rr(x19, x10),         %% n = quotient
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ne, 0),
        {reloc, arm64_cond_branch19, DivLoopLbl, -4},

        %% Print: write(1, sp+x20, 128-x20)
        {label, PrintLbl},
        ?ENC:encode_add_imm(x12, sp, 0),      %% x12 = sp
        ?ENC:encode_add_rrr(x1, x12, x20),    %% x1 = sp + x20 (buf start)
        ?ENC:encode_mov_imm64(x2, 128),
        ?ENC:encode_sub_rrr(x2, x2, x20),     %% x2 = 128 - x20 (len)
        ?ENC:encode_mov_imm64(x0, 1),         %% fd = stdout
        ?ENC:encode_mov_imm64(SysNumReg, WriteNum),
        ?ENC:encode_svc(SvcImm),

        %% Restore all saved registers
        ?ENC:encode_ldr(x0, sp, 16),
        ?ENC:encode_ldr(x1, sp, 24),
        ?ENC:encode_ldr(x2, sp, 32),
        ?ENC:encode_ldr(x9, sp, 40),
        ?ENC:encode_ldr(x10, sp, 48),
        ?ENC:encode_ldr(x11, sp, 56),
        ?ENC:encode_ldr(x12, sp, 64),
        ?ENC:encode_ldr(SysNumReg, sp, 72),

        %% Restore x19, x20 and deallocate 128-byte frame
        ?ENC:encode_ldp_post(x19, x20, sp, 128)
    ]);

%%====================================================================
%% Phase 6a: String instructions
%%====================================================================

%% STRING_LIT: Load address of string literal from data section.
%% The string bytes are stored in the data section; the IR compiler
%% adds them via add_data/4. Here we emit LEA (ADRP+ADD) for the
%% data ref, then store {pointer, length} as a fat pointer.
%% Result: Dst = pointer to {ptr, len} pair on heap.
%%
%% Memory layout of string value (16 bytes on heap):
%%   [0..7]  = pointer to data bytes
%%   [8..15] = length (integer)
lower_instruction({string_lit, {preg, Dst}, {data_ref, Name}, Len},
                  _FnName, _FS, _Fmt, _UC) ->
    %% Allocate 16 bytes on heap for fat pointer
    AllocCode = vbeam_native_alloc:emit_alloc(arm64, Dst, 16),
    %% Load address of string data into x16
    LeaCode = [?ENC:encode_adrp(x16, 0),
               {reloc, adrp_page21, Name, -4},
               ?ENC:encode_add_imm(x16, x16, 0),
               {reloc, add_pageoff12, Name, -4}],
    %% Store pointer and length into heap-allocated pair
    StoreCode = [?ENC:encode_str(x16, Dst, 0),             %% [Dst+0] = ptr
                 ?ENC:encode_mov_imm64(x17, Len),
                 ?ENC:encode_str(x17, Dst, 8)],             %% [Dst+8] = len
    lists:flatten([AllocCode, LeaCode, StoreCode]);

%% STRING_LIT (2-arg form): just load data address into register
lower_instruction({string_lit, {preg, Dst}, {data_ref, Name}},
                  _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_adrp(Dst, 0),
     {reloc, adrp_page21, Name, -4},
     ?ENC:encode_add_imm(Dst, Dst, 0),
     {reloc, add_pageoff12, Name, -4}];

%% STRING_LEN: Get string length from fat pointer.
%% Src points to {ptr, len} pair; load len field.
lower_instruction({string_len, {preg, Dst}, {preg, Src}},
                  _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_ldr(Dst, Src, 8)];  %% Dst = [Src+8] (len field)

%% STRING_CMP: Compare two strings byte-by-byte.
%% A, B point to {ptr, len} pairs. Result: -1, 0, or 1 in Dst.
%% Uses a loop comparing bytes; if lengths differ after common prefix,
%% shorter string is "less than".
lower_instruction({string_cmp, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__scmp_loop_", Uid/binary>>,
    EqLbl = <<"__scmp_eq_", Uid/binary>>,
    LtLbl = <<"__scmp_lt_", Uid/binary>>,
    GtLbl = <<"__scmp_gt_", Uid/binary>>,
    DoneLbl = <<"__scmp_done_", Uid/binary>>,
    lists:flatten([
        %% x9 = A.ptr, x10 = A.len, x11 = B.ptr, x12 = B.len
        ?ENC:encode_ldr(x9, A, 0),
        ?ENC:encode_ldr(x10, A, 8),
        ?ENC:encode_ldr(x11, B, 0),
        ?ENC:encode_ldr(x12, B, 8),
        %% x13 = min(A.len, B.len) = common prefix length
        ?ENC:encode_cmp_rr(x10, x12),
        ?ENC:encode_mov_rr(x13, x10),         %% assume A.len <= B.len
        ?ENC:encode_b_cond(le, 0),
        {reloc, arm64_cond_branch19, <<"__scmp_minok_", Uid/binary>>, -4},
        ?ENC:encode_mov_rr(x13, x12),         %% B.len is shorter
        {label, <<"__scmp_minok_", Uid/binary>>},
        %% x14 = 0 (loop index)
        ?ENC:encode_mov_imm64(x14, 0),

        %% Loop: compare byte at index x14
        {label, LoopLbl},
        ?ENC:encode_cmp_rr(x14, x13),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, EqLbl, -4},
        %% Load bytes: x15 = A[x14], x16 = B[x14]
        ?ENC:encode_add_rrr(x15, x9, x14),
        ?ENC:encode_ldrb(x15, x15, 0),
        ?ENC:encode_add_rrr(x16, x11, x14),
        ?ENC:encode_ldrb(x16, x16, 0),
        ?ENC:encode_cmp_rr(x15, x16),
        ?ENC:encode_b_cond(lt, 0),
        {reloc, arm64_cond_branch19, LtLbl, -4},
        ?ENC:encode_b_cond(gt, 0),
        {reloc, arm64_cond_branch19, GtLbl, -4},
        ?ENC:encode_add_imm(x14, x14, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, LoopLbl, -4},

        %% Common prefix equal — compare lengths
        {label, EqLbl},
        ?ENC:encode_cmp_rr(x10, x12),
        ?ENC:encode_b_cond(lt, 0),
        {reloc, arm64_cond_branch19, LtLbl, -4},
        ?ENC:encode_b_cond(gt, 0),
        {reloc, arm64_cond_branch19, GtLbl, -4},
        %% Equal
        ?ENC:encode_mov_imm64(Dst, 0),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        {label, LtLbl},
        ?ENC:encode_mov_imm64(Dst, -1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        {label, GtLbl},
        ?ENC:encode_mov_imm64(Dst, 1),

        {label, DoneLbl}
    ]);

%% STRING_CONCAT: Concatenate two strings.
%% A, B are fat pointers {ptr, len}. Result is a new fat pointer in Dst.
lower_instruction({string_concat, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    CopyALbl = <<"__scat_cpA_", Uid/binary>>,
    CopyADone = <<"__scat_cpAd_", Uid/binary>>,
    CopyBLbl = <<"__scat_cpB_", Uid/binary>>,
    CopyBDone = <<"__scat_cpBd_", Uid/binary>>,
    lists:flatten([
        %% Load A and B fields
        %% x9=A.ptr, x10=A.len, x11=B.ptr, x12=B.len
        ?ENC:encode_ldr(x9, A, 0),
        ?ENC:encode_ldr(x10, A, 8),
        ?ENC:encode_ldr(x11, B, 0),
        ?ENC:encode_ldr(x12, B, 8),
        %% x13 = total_len = A.len + B.len
        ?ENC:encode_add_rrr(x13, x10, x12),

        %% Allocate data buffer: x14 = alloc(total_len)
        %% We use heap reg directly (x28)
        ?ENC:encode_mov_rr(x14, x28),
        %% Advance heap by aligned total_len
        ?ENC:encode_add_imm(x15, x13, 7),
        ?ENC:encode_mov_imm64(x17, -8),
        ?ENC:encode_and_rrr(x15, x15, x17),
        ?ENC:encode_add_rrr(x28, x28, x15),

        %% Copy A bytes: memcpy(x14, x9, x10)
        ?ENC:encode_mov_imm64(x15, 0),  %% index
        {label, CopyALbl},
        ?ENC:encode_cmp_rr(x15, x10),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, CopyADone, -4},
        ?ENC:encode_add_rrr(x16, x9, x15),
        ?ENC:encode_ldrb(x16, x16, 0),
        ?ENC:encode_add_rrr(x17, x14, x15),
        ?ENC:encode_strb(x16, x17, 0),
        ?ENC:encode_add_imm(x15, x15, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, CopyALbl, -4},
        {label, CopyADone},

        %% Copy B bytes: memcpy(x14+A.len, x11, x12)
        ?ENC:encode_mov_imm64(x15, 0),  %% index
        {label, CopyBLbl},
        ?ENC:encode_cmp_rr(x15, x12),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, CopyBDone, -4},
        ?ENC:encode_add_rrr(x16, x11, x15),
        ?ENC:encode_ldrb(x16, x16, 0),
        ?ENC:encode_add_rrr(x17, x14, x10),  %% dest = data_buf + A.len
        ?ENC:encode_add_rrr(x17, x17, x15),  %% + index
        ?ENC:encode_strb(x16, x17, 0),
        ?ENC:encode_add_imm(x15, x15, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, CopyBLbl, -4},
        {label, CopyBDone},

        %% Allocate fat pointer (16 bytes) for result
        vbeam_native_alloc:emit_alloc(arm64, Dst, 16),
        %% Store {data_ptr, total_len}
        ?ENC:encode_str(x14, Dst, 0),
        ?ENC:encode_str(x13, Dst, 8)
    ]);

%% PRINT_STR: Print string to stdout via write syscall.
%% Src is a fat pointer {ptr, len}.
lower_instruction({print_str, {preg, Src}}, _FnName, _FS, Fmt, _UC) ->
    {SysNumReg, WriteNum, SvcImm} = case Fmt of
        macho -> {x16, 4, 16#80};
        _     -> {x8, 64, 0}
    end,
    lists:flatten([
        %% Save registers we'll clobber (x0, x1, x2, x8, x19, x20)
        %% Stack frame: 56 bytes (16-aligned to 64)
        %%   [sp+0..15]  = x19, x20 (via STP)
        %%   [sp+16]     = x0
        %%   [sp+24]     = x1
        %%   [sp+32]     = x2
        %%   [sp+40]     = SysNumReg (x8 or x16)
        ?ENC:encode_stp_pre(x19, x20, sp, -64),
        ?ENC:encode_str(x0, sp, 16),
        ?ENC:encode_str(x1, sp, 24),
        ?ENC:encode_str(x2, sp, 32),
        ?ENC:encode_str(SysNumReg, sp, 40),

        %% Load string data into temps first (Src could be x0, x1, or x2!)
        ?ENC:encode_ldr(x19, Src, 0),  %% x19 = buf (ptr)
        ?ENC:encode_ldr(x20, Src, 8),  %% x20 = len
        ?ENC:encode_mov_rr(x1, x19),   %% x1 = buf
        ?ENC:encode_mov_rr(x2, x20),   %% x2 = len
        ?ENC:encode_mov_imm64(x0, 1),  %% fd = stdout
        ?ENC:encode_mov_imm64(SysNumReg, WriteNum),
        ?ENC:encode_svc(SvcImm),

        %% Restore
        ?ENC:encode_ldr(SysNumReg, sp, 40),
        ?ENC:encode_ldr(x2, sp, 32),
        ?ENC:encode_ldr(x1, sp, 24),
        ?ENC:encode_ldr(x0, sp, 16),
        ?ENC:encode_ldp_post(x19, x20, sp, 64)
    ]);

%%====================================================================
%% Phase 6b: Array instructions
%%====================================================================

%% ARRAY_NEW: Allocate a new array with given element size and initial capacity.
%% Result: Dst points to array header {ptr, len=0, cap} (24 bytes).
%% Element buffer: cap * elem_size bytes.
lower_instruction({array_new, {preg, Dst}, {imm, ElemSize}, {imm, InitCap}},
                  _FnName, _FS, _Fmt, _UC) ->
    BufSize = ElemSize * InitCap,
    lists:flatten([
        %% Allocate element buffer
        vbeam_native_alloc:emit_alloc(arm64, x16, BufSize),
        %% Allocate header (24 bytes)
        vbeam_native_alloc:emit_alloc(arm64, Dst, 24),
        %% Store header fields
        ?ENC:encode_str(x16, Dst, 0),                   %% [Dst+0]  = ptr
        ?ENC:encode_mov_imm64(x17, 0),
        ?ENC:encode_str(x17, Dst, 8),                   %% [Dst+8]  = len (0)
        ?ENC:encode_mov_imm64(x17, InitCap),
        ?ENC:encode_str(x17, Dst, 16)                   %% [Dst+16] = cap
    ]);

%% ARRAY_GET: Load element at index from array.
%% Arr points to header {ptr, len, cap}. Index is in register.
%% Element address = ptr + index * elem_size
lower_instruction({array_get, {preg, Dst}, {preg, Arr}, {preg, Idx}, {imm, ElemSize}},
                  _FnName, _FS, _Fmt, _UC) ->
    lists:flatten([
        ?ENC:encode_ldr(x16, Arr, 0),                   %% x16 = arr.ptr
        ?ENC:encode_mov_imm64(x17, ElemSize),
        ?ENC:encode_mul(x17, Idx, x17),                 %% x17 = idx * elem_size
        ?ENC:encode_add_rrr(x16, x16, x17),             %% x16 = ptr + offset
        ?ENC:encode_ldr(Dst, x16, 0)                    %% Dst = *x16
    ]);

%% ARRAY_GET with immediate index
lower_instruction({array_get, {preg, Dst}, {preg, Arr}, {imm, Idx}, {imm, ElemSize}},
                  _FnName, _FS, _Fmt, _UC) ->
    Offset = Idx * ElemSize,
    lists:flatten([
        ?ENC:encode_ldr(x16, Arr, 0),                   %% x16 = arr.ptr
        if Offset >= 0, Offset < 32768, Offset rem 8 =:= 0 ->
            ?ENC:encode_ldr(Dst, x16, Offset);
           true ->
            [?ENC:encode_mov_imm64(x17, Offset),
             ?ENC:encode_add_rrr(x16, x16, x17),
             ?ENC:encode_ldr(Dst, x16, 0)]
        end
    ]);

%% ARRAY_SET with immediate index.
lower_instruction({array_set, {preg, Arr}, {imm, Idx}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _FS, _Fmt, _UC) ->
    Offset = Idx * ElemSize,
    lists:flatten([
        ?ENC:encode_ldr(x16, Arr, 0),                   %% x16 = arr.ptr
        if Offset >= 0, Offset < 32768, Offset rem 8 =:= 0 ->
            ?ENC:encode_str(Val, x16, Offset);
           true ->
            [?ENC:encode_mov_imm64(x17, Offset),
             ?ENC:encode_add_rrr(x16, x16, x17),
             ?ENC:encode_str(Val, x16, 0)]
        end
    ]);

%% ARRAY_SET with register index.
lower_instruction({array_set, {preg, Arr}, {preg, Idx}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _FS, _Fmt, _UC) ->
    lists:flatten([
        ?ENC:encode_ldr(x16, Arr, 0),                   %% x16 = arr.ptr
        ?ENC:encode_mov_imm64(x17, ElemSize),
        ?ENC:encode_mul(x17, Idx, x17),                 %% x17 = idx * elem_size
        ?ENC:encode_add_rrr(x16, x16, x17),
        ?ENC:encode_str(Val, x16, 0)                    %% *x16 = Val
    ]);

%% ARRAY_LEN: Get array length.
lower_instruction({array_len, {preg, Dst}, {preg, Arr}},
                  _FnName, _FS, _Fmt, _UC) ->
    [?ENC:encode_ldr(Dst, Arr, 8)];  %% len is at offset 8

%% ARRAY_APPEND: Append element to array (may need to grow).
%% If len < cap, just store and increment len.
%% If len == cap, double capacity and copy.
lower_instruction({array_append, {preg, Dst}, {preg, Arr}, {preg, Val}, {imm, ElemSize}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NeedGrowLbl = <<"__aapp_grow_", Uid/binary>>,
    StoreLbl = <<"__aapp_store_", Uid/binary>>,
    CopyLoopLbl = <<"__aapp_cpy_", Uid/binary>>,
    CopyDoneLbl = <<"__aapp_cpyd_", Uid/binary>>,
    lists:flatten([
        %% Load header fields
        ?ENC:encode_ldr(x9, Arr, 0),                    %% x9  = ptr
        ?ENC:encode_ldr(x10, Arr, 8),                   %% x10 = len
        ?ENC:encode_ldr(x11, Arr, 16),                  %% x11 = cap

        %% Check if we need to grow
        ?ENC:encode_cmp_rr(x10, x11),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, NeedGrowLbl, -4},

        %% No grow needed: store at ptr + len * elem_size
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, StoreLbl, -4},

        %% Grow: double capacity, allocate new buffer, copy
        {label, NeedGrowLbl},
        %% new_cap = cap * 2 (or 4 if cap == 0)
        ?ENC:encode_cmp_imm(x11, 0),
        ?ENC:encode_b_cond(ne, 0),
        {reloc, arm64_cond_branch19, <<"__aapp_notz_", Uid/binary>>, -4},
        ?ENC:encode_mov_imm64(x11, 2),  %% if cap==0, set to 2
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, <<"__aapp_dbl_", Uid/binary>>, -4},
        {label, <<"__aapp_notz_", Uid/binary>>},
        ?ENC:encode_add_rrr(x11, x11, x11),            %% cap *= 2
        {label, <<"__aapp_dbl_", Uid/binary>>},

        %% Allocate new buffer: x12 = alloc(new_cap * elem_size)
        ?ENC:encode_mov_imm64(x13, ElemSize),
        ?ENC:encode_mul(x14, x11, x13),                %% x14 = new_cap * elem_size
        %% Bump allocate x14 bytes
        ?ENC:encode_mov_rr(x12, x28),                  %% x12 = new_buf
        ?ENC:encode_add_imm(x15, x14, 7),
        ?ENC:encode_mov_imm64(x17, -8),
        ?ENC:encode_and_rrr(x15, x15, x17),
        ?ENC:encode_add_rrr(x28, x28, x15),

        %% Copy old data: x15 = old_len * elem_size bytes from x9 to x12
        ?ENC:encode_mul(x15, x10, x13),                %% x15 = len * elem_size
        ?ENC:encode_mov_imm64(x14, 0),                 %% index
        {label, CopyLoopLbl},
        ?ENC:encode_cmp_rr(x14, x15),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, CopyDoneLbl, -4},
        ?ENC:encode_add_rrr(x16, x9, x14),
        ?ENC:encode_ldrb(x16, x16, 0),
        ?ENC:encode_add_rrr(x17, x12, x14),
        ?ENC:encode_strb(x16, x17, 0),
        ?ENC:encode_add_imm(x14, x14, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, CopyLoopLbl, -4},
        {label, CopyDoneLbl},

        %% Update array header with new buffer
        ?ENC:encode_mov_rr(x9, x12),                   %% ptr = new_buf
        ?ENC:encode_str(x12, Arr, 0),                  %% arr.ptr = new_buf
        ?ENC:encode_str(x11, Arr, 16),                 %% arr.cap = new_cap

        %% Store the new element
        {label, StoreLbl},
        ?ENC:encode_mov_imm64(x13, ElemSize),
        ?ENC:encode_mul(x14, x10, x13),                %% offset = len * elem_size
        ?ENC:encode_add_rrr(x16, x9, x14),
        ?ENC:encode_str(Val, x16, 0),                  %% ptr[len] = val

        %% Increment length
        ?ENC:encode_add_imm(x10, x10, 1),
        ?ENC:encode_str(x10, Arr, 8),                  %% arr.len++

        %% Dst = Arr (same header, updated in place)
        ?ENC:encode_mov_rr(Dst, Arr)
    ]);

%%====================================================================
%% Phase 6c: Struct instructions
%%====================================================================

%% STRUCT_NEW: Allocate a struct of given byte size on the heap.
lower_instruction({struct_new, {preg, Dst}, {imm, Size}},
                  _FnName, _FS, _Fmt, _UC) ->
    vbeam_native_alloc:emit_alloc(arm64, Dst, Size);

%% FIELD_GET: Load a field at byte offset from struct pointer.
lower_instruction({field_get, {preg, Dst}, {preg, Struct}, {imm, Offset}},
                  _FnName, _FS, _Fmt, _UC) ->
    if Offset >= 0, Offset rem 8 =:= 0, Offset div 8 < 4096 ->
        [?ENC:encode_ldr(Dst, Struct, Offset)];
       true ->
        [?ENC:encode_mov_imm64(x16, Offset),
         ?ENC:encode_add_rrr(x16, Struct, x16),
         ?ENC:encode_ldr(Dst, x16, 0)]
    end;

%% FIELD_SET: Store a value at byte offset in struct.
lower_instruction({field_set, {preg, Struct}, {imm, Offset}, {preg, Val}},
                  _FnName, _FS, _Fmt, _UC) ->
    if Offset >= 0, Offset rem 8 =:= 0, Offset div 8 < 4096 ->
        [?ENC:encode_str(Val, Struct, Offset)];
       true ->
        [?ENC:encode_mov_imm64(x16, Offset),
         ?ENC:encode_add_rrr(x16, Struct, x16),
         ?ENC:encode_str(Val, x16, 0)]
    end;

%%====================================================================
%% Phase 6d: Map instructions (sorted-array implementation)
%%====================================================================

%% MAP_NEW: Create empty map. Same header layout as array: {ptr, len=0, cap=8}
lower_instruction({map_new, {preg, Dst}}, _FnName, _FS, _Fmt, _UC) ->
    %% Each entry = {key, value} = 16 bytes
    %% Initial capacity: 8 entries = 128 bytes
    InitCap = 8,
    BufSize = InitCap * 16,
    lists:flatten([
        vbeam_native_alloc:emit_alloc(arm64, x16, BufSize), %% entry buffer
        vbeam_native_alloc:emit_alloc(arm64, Dst, 24),      %% header
        ?ENC:encode_str(x16, Dst, 0),                       %% ptr
        ?ENC:encode_mov_imm64(x17, 0),
        ?ENC:encode_str(x17, Dst, 8),                       %% len = 0
        ?ENC:encode_mov_imm64(x17, InitCap),
        ?ENC:encode_str(x17, Dst, 16)                       %% cap = 8
    ]);

%% MAP_GET: Linear scan for key in sorted array.
%% Map = {entries_ptr, len, cap}. Each entry = {key(8), value(8)}.
%% Dst = value if found, 0 if not found.
lower_instruction({map_get, {preg, Dst}, {preg, Map}, {preg, Key}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__mg_loop_", Uid/binary>>,
    FoundLbl = <<"__mg_found_", Uid/binary>>,
    NotFoundLbl = <<"__mg_nf_", Uid/binary>>,
    DoneLbl = <<"__mg_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_ldr(x9, Map, 0),                    %% x9  = entries ptr
        ?ENC:encode_ldr(x10, Map, 8),                   %% x10 = len
        ?ENC:encode_mov_imm64(x11, 0),                  %% x11 = index

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(x11, x10),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, NotFoundLbl, -4},

        %% Load entry[x11].key
        ?ENC:encode_mov_imm64(x12, 16),
        ?ENC:encode_mul(x12, x11, x12),                %% offset = index * 16
        ?ENC:encode_add_rrr(x13, x9, x12),             %% entry addr
        ?ENC:encode_ldr(x14, x13, 0),                  %% x14 = entry.key

        ?ENC:encode_cmp_rr(x14, Key),
        ?ENC:encode_b_cond(eq, 0),
        {reloc, arm64_cond_branch19, FoundLbl, -4},

        ?ENC:encode_add_imm(x11, x11, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, LoopLbl, -4},

        {label, FoundLbl},
        ?ENC:encode_ldr(Dst, x13, 8),                  %% Dst = entry.value
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        {label, NotFoundLbl},
        ?ENC:encode_mov_imm64(Dst, 0),                 %% not found → 0

        {label, DoneLbl}
    ]);

%% MAP_PUT: Insert or update key-value pair.
%% Scans for key; if found, updates value. If not, appends.
lower_instruction({map_put, {preg, Dst}, {preg, Map}, {preg, Key}, {preg, Val}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__mp_loop_", Uid/binary>>,
    FoundLbl = <<"__mp_found_", Uid/binary>>,
    AppendLbl = <<"__mp_append_", Uid/binary>>,
    CapFullLbl = <<"__mp_capfull_", Uid/binary>>,
    DoneLbl = <<"__mp_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_ldr(x9, Map, 0),                    %% entries ptr
        ?ENC:encode_ldr(x10, Map, 8),                   %% len
        ?ENC:encode_ldr(x11, Map, 16),                  %% cap
        ?ENC:encode_mov_imm64(x12, 0),                  %% index

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(x12, x10),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, AppendLbl, -4},

        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x13, x12, x13),
        ?ENC:encode_add_rrr(x14, x9, x13),             %% entry addr
        ?ENC:encode_ldr(x15, x14, 0),                  %% entry.key

        ?ENC:encode_cmp_rr(x15, Key),
        ?ENC:encode_b_cond(eq, 0),
        {reloc, arm64_cond_branch19, FoundLbl, -4},

        ?ENC:encode_add_imm(x12, x12, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, LoopLbl, -4},

        %% Found: update value
        {label, FoundLbl},
        ?ENC:encode_str(Val, x14, 8),                  %% entry.value = Val
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        %% Append new entry at end (with capacity check)
        {label, AppendLbl},
        %% Bounds check: if len >= cap, fail (growth would go here)
        ?ENC:encode_cmp_rr(x10, x11),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, CapFullLbl, -4},

        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x13, x10, x13),                %% offset = len * 16
        ?ENC:encode_add_rrr(x14, x9, x13),
        ?ENC:encode_str(Key, x14, 0),                  %% entry.key = Key
        ?ENC:encode_str(Val, x14, 8),                  %% entry.value = Val
        ?ENC:encode_add_imm(x10, x10, 1),
        ?ENC:encode_str(x10, Map, 8),                  %% len++
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        {label, CapFullLbl},
        %% Map capacity exhausted - grow and reallocate
        %% Double capacity, allocate new buffer, copy entries
        ?ENC:encode_add_rrr(x11, x11, x11),            %% new_cap = cap * 2
        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x14, x11, x13),                %% new_size = new_cap * 16
        %% Allocate new buffer
        ?ENC:encode_mov_rr(x12, x28),                  %% x12 = new_buf
        ?ENC:encode_add_imm(x15, x14, 7),
        ?ENC:encode_mov_imm64(x17, -8),
        ?ENC:encode_and_rrr(x15, x15, x17),
        ?ENC:encode_add_rrr(x28, x28, x15),            %% bump heap
        %% Copy old entries byte-by-byte
        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x15, x10, x13),                %% old_size = len * 16 bytes
        ?ENC:encode_mov_imm64(x14, 0),                 %% copy index
        %% Copy loop
        {label, <<"__mp_cpy_", Uid/binary>>},
        ?ENC:encode_cmp_rr(x14, x15),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, <<"__mp_cpyd_", Uid/binary>>, -4},
        ?ENC:encode_add_rrr(x16, x9, x14),
        ?ENC:encode_ldrb(x16, x16, 0),
        ?ENC:encode_add_rrr(x17, x12, x14),
        ?ENC:encode_strb(x16, x17, 0),
        ?ENC:encode_add_imm(x14, x14, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, <<"__mp_cpy_", Uid/binary>>, -4},
        {label, <<"__mp_cpyd_", Uid/binary>>},
        %% Update map header with new buffer
        ?ENC:encode_mov_rr(x9, x12),                   %% ptr = new_buf
        ?ENC:encode_str(x12, Map, 0),                  %% map.ptr = new_buf
        ?ENC:encode_str(x11, Map, 16),                 %% map.cap = new_cap
        %% Fall through to store entry in new buffer

        %% Store new entry at end (common path after growth)
        {label, <<"__mp_store_", Uid/binary>>},
        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x13, x10, x13),                %% offset = len * 16
        ?ENC:encode_add_rrr(x14, x9, x13),             %% entry addr in (possibly new) buf
        ?ENC:encode_str(Key, x14, 0),                  %% entry.key = Key
        ?ENC:encode_str(Val, x14, 8),                  %% entry.value = Val
        ?ENC:encode_add_imm(x10, x10, 1),
        ?ENC:encode_str(x10, Map, 8),                  %% len++

        {label, DoneLbl},
        ?ENC:encode_mov_rr(Dst, Map)                   %% return same map
    ]);

%% MAP_DELETE: Remove key from map (shift entries down).
lower_instruction({map_delete, {preg, Dst}, {preg, Map}, {preg, Key}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    LoopLbl = <<"__md_loop_", Uid/binary>>,
    FoundLbl = <<"__md_found_", Uid/binary>>,
    ShiftLbl = <<"__md_shift_", Uid/binary>>,
    ShiftDone = <<"__md_shd_", Uid/binary>>,
    DoneLbl = <<"__md_done_", Uid/binary>>,
    lists:flatten([
        ?ENC:encode_ldr(x9, Map, 0),                    %% entries ptr
        ?ENC:encode_ldr(x10, Map, 8),                   %% len
        ?ENC:encode_mov_imm64(x11, 0),                  %% index

        {label, LoopLbl},
        ?ENC:encode_cmp_rr(x11, x10),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, DoneLbl, -4},

        ?ENC:encode_mov_imm64(x12, 16),
        ?ENC:encode_mul(x12, x11, x12),
        ?ENC:encode_add_rrr(x13, x9, x12),
        ?ENC:encode_ldr(x14, x13, 0),

        ?ENC:encode_cmp_rr(x14, Key),
        ?ENC:encode_b_cond(eq, 0),
        {reloc, arm64_cond_branch19, FoundLbl, -4},

        ?ENC:encode_add_imm(x11, x11, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, LoopLbl, -4},

        %% Found: shift entries [index+1..len-1] down by one
        {label, FoundLbl},
        ?ENC:encode_add_imm(x12, x11, 1),              %% x12 = index+1
        {label, ShiftLbl},
        ?ENC:encode_cmp_rr(x12, x10),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, ShiftDone, -4},
        %% Copy entry[x12] to entry[x12-1]
        ?ENC:encode_mov_imm64(x13, 16),
        ?ENC:encode_mul(x14, x12, x13),
        ?ENC:encode_add_rrr(x14, x9, x14),             %% src entry
        ?ENC:encode_ldr(x15, x14, 0),                  %% key
        ?ENC:encode_ldr(x16, x14, 8),                  %% value
        ?ENC:encode_sub_imm(x17, x14, 16),             %% dst entry
        ?ENC:encode_str(x15, x17, 0),
        ?ENC:encode_str(x16, x17, 8),
        ?ENC:encode_add_imm(x12, x12, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, ShiftLbl, -4},
        {label, ShiftDone},
        %% Decrement len
        ?ENC:encode_sub_imm(x10, x10, 1),
        ?ENC:encode_str(x10, Map, 8),

        {label, DoneLbl},
        ?ENC:encode_mov_rr(Dst, Map)
    ]);

%%====================================================================
%% Phase 6e: Float instructions
%%====================================================================

%% For now, floats are stored as 64-bit integers (bit patterns).
%% Full FPU support would use d0-d31 registers. This is a placeholder
%% that handles float arithmetic via integer-encoded doubles.
%%
%% TODO: When full FPU support is added, these should use FADD/FSUB/FMUL/FDIV
%% ARM64 SIMD instructions and d0-d31 registers.

%% INT_TO_FLOAT: Convert integer to float (double).
%% Uses SCVTF instruction: SCVTF Dd, Xn
lower_instruction({int_to_float, {preg, Dst}, {preg, Src}},
                  _FnName, _FS, _Fmt, _UC) ->
    %% SCVTF D0, Xn: 1 00 11110 01 1 00010 000000 Rn Rd
    %% Then FMOV Xd, D0: 1 00 11110 01 1 00110 000000 Rd Rn(=0)
    SrcCode = ?ENC:reg_code(Src),
    DstCode = ?ENC:reg_code(Dst),
    %% SCVTF D0, Src
    ScvtfInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00010 bsl 16)
        bor (2#000000 bsl 10) bor (SrcCode bsl 5) bor 0,
    %% FMOV Xd, D0
    FmovInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00110 bsl 16)
        bor (2#000000 bsl 10) bor (0 bsl 5) bor DstCode,
    [<<ScvtfInst:32/little>>, <<FmovInst:32/little>>];

%% FLOAT_TO_INT: Convert float to integer (truncate).
lower_instruction({float_to_int, {preg, Dst}, {preg, Src}},
                  _FnName, _FS, _Fmt, _UC) ->
    SrcCode = ?ENC:reg_code(Src),
    DstCode = ?ENC:reg_code(Dst),
    %% FMOV D0, Xn
    FmovInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00111 bsl 16)
        bor (2#000000 bsl 10) bor (SrcCode bsl 5) bor 0,
    %% FCVTZS Xd, D0
    FcvtInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#11000 bsl 16)
        bor (2#000000 bsl 10) bor (0 bsl 5) bor DstCode,
    [<<FmovInst:32/little>>, <<FcvtInst:32/little>>];

%%====================================================================
%% Phase 6f: Method calls
%%====================================================================

%% METHOD_CALL: Call Type__Method with receiver as first argument.
%% Lowered to a regular function call with mangled name.
lower_instruction({method_call, {preg, Dst}, ReceiverType, MethodName, Args},
                  _FnName, _FS, _Fmt, UC) when is_binary(ReceiverType),
                                                 is_binary(MethodName),
                                                 is_list(Args) ->
    MangledName = <<ReceiverType/binary, "__", MethodName/binary>>,
    %% Move args to registers x0, x1, ... (AAPCS64)
    ArgRegs = [x0, x1, x2, x3, x4, x5, x6, x7],
    ArgMoves = move_args_to_regs(Args, ArgRegs, length(UC)),
    CallCode = [?ENC:encode_bl(0),
                {reloc, arm64_branch26, MangledName, -4}],
    %% Result is in x0; move to Dst if needed
    ResultMove = case Dst of
        x0 -> [];
        _  -> [?ENC:encode_mov_rr(Dst, x0)]
    end,
    lists:flatten([ArgMoves, CallCode, ResultMove]);

%% PRINT_FLOAT: Print a float value to stdout.
%% For now, converts float to integer and prints that (approximate).
%% A proper implementation would need full float-to-decimal conversion.
lower_instruction({print_float, {preg, Src}}, FnName, FS, Fmt, UC) ->
    SrcCode = ?ENC:reg_code(Src),
    %% FMOV D0, Xsrc
    FmovInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00111 bsl 16)
        bor (2#000000 bsl 10) bor (SrcCode bsl 5) bor 0,
    %% FCVTZS X16, D0
    FcvtInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#11000 bsl 16)
        bor (2#000000 bsl 10) bor (0 bsl 5) bor ?ENC:reg_code(x16),
    FloatToInt = [<<FmovInst:32/little>>, <<FcvtInst:32/little>>],
    %% Then print the integer part
    PrintInt = lower_instruction({print_int, {preg, x16}}, FnName, FS, Fmt, UC),
    FloatToInt ++ PrintInt;

%%====================================================================
%% Phase 6f: Type conversion instructions
%%====================================================================

%% INT_TO_STR: Convert integer to string (fat pointer {ptr, len}).
%% Similar to print_int but stores result in heap buffer.
%% Dst gets a fat pointer. Uses x9-x15 as scratch.
lower_instruction({int_to_str, {preg, Dst}, {preg, Src}},
                  _FnName, _FS, _Fmt, _UC) ->
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    NonZeroLbl = <<"__its_nz_", Uid/binary>>,
    PositiveLbl = <<"__its_pos_", Uid/binary>>,
    DivLoopLbl = <<"__its_div_", Uid/binary>>,
    DoneLbl = <<"__its_done_", Uid/binary>>,
    %% Buffer: 24 bytes on stack (enough for 64-bit int + sign + NUL)
    %% We'll use a stack-allocated temp buffer, then copy to heap.
    lists:flatten([
        %% Save x19, x20 and allocate 48-byte frame (16-aligned)
        ?ENC:encode_stp_pre(x19, x20, sp, -48),

        %% x19 = input value
        ?ENC:encode_mov_rr(x19, Src),

        %% x20 = 46 (write position in buffer at sp+0..47, working backward from 47)
        ?ENC:encode_mov_imm64(x20, 47),

        %% Check zero
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ne, 0),
        {reloc, arm64_cond_branch19, NonZeroLbl, -4},

        %% Zero case: store '0' at sp+46
        ?ENC:encode_mov_imm64(x9, 48),    %% '0'
        ?ENC:encode_strb(x9, sp, 46),
        ?ENC:encode_mov_imm64(x20, 46),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, DoneLbl, -4},

        %% Nonzero
        {label, NonZeroLbl},

        %% Check negative
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, PositiveLbl, -4},

        %% Save sign in x14 (before clobbering Src)
        ?ENC:encode_cmp_imm(Src, 0),
        ?ENC:encode_mov_imm64(x14, 0),        %% x14 = 0 (positive)
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, <<"__its_signok_", Uid/binary>>, -4},
        ?ENC:encode_mov_imm64(x14, 1),        %% x14 = 1 (negative)
        {label, <<"__its_signok_", Uid/binary>>},

        %% Negate value (we'll add '-' at the end)
        ?ENC:encode_neg(x19, x19),

        %% Positive (or negated)
        {label, PositiveLbl},

        %% Digit extraction loop (store digits right-to-left)
        {label, DivLoopLbl},
        ?ENC:encode_sub_imm(x20, x20, 1),     %% pos--
        ?ENC:encode_mov_imm64(x9, 10),
        ?ENC:encode_sdiv(x10, x19, x9),       %% x10 = x19 / 10
        ?ENC:encode_msub(x11, x10, x9, x19),  %% x11 = x19 % 10
        ?ENC:encode_add_imm(x11, x11, 48),    %% x11 += '0'
        %% Store digit at sp + x20
        ?ENC:encode_add_imm(x12, sp, 0),      %% x12 = sp
        ?ENC:encode_add_rrr(x12, x12, x20),   %% x12 = sp + x20
        ?ENC:encode_strb(x11, x12, 0),
        ?ENC:encode_mov_rr(x19, x10),         %% n = quotient
        ?ENC:encode_cmp_imm(x19, 0),
        ?ENC:encode_b_cond(ne, 0),
        {reloc, arm64_cond_branch19, DivLoopLbl, -4},

        %% If original was negative (check saved x14), prepend '-'
        ?ENC:encode_cmp_imm(x14, 0),
        ?ENC:encode_b_cond(eq, 0),
        {reloc, arm64_cond_branch19, DoneLbl, -4},
        ?ENC:encode_sub_imm(x20, x20, 1),
        ?ENC:encode_mov_imm64(x9, 45),        %% '-'
        ?ENC:encode_add_imm(x12, sp, 0),
        ?ENC:encode_add_rrr(x12, x12, x20),
        ?ENC:encode_strb(x9, x12, 0),

        {label, DoneLbl},

        %% x20 = start position, length = 48 - x20
        %% Compute length into x13
        ?ENC:encode_mov_imm64(x13, 48),
        ?ENC:encode_sub_rrr(x13, x13, x20),   %% x13 = len

        %% Allocate buffer on heap for the string data: x14 = alloc(len)
        %% Round up to 8-byte alignment
        ?ENC:encode_add_imm(x9, x13, 7),
        ?ENC:encode_mov_imm64(x10, -8),
        ?ENC:encode_and_rrr(x9, x9, x10),
        ?ENC:encode_mov_rr(x14, x28),         %% x14 = heap ptr (data buf)
        ?ENC:encode_add_rrr(x28, x28, x9),    %% bump heap

        %% Copy digits from stack buf (sp+x20) to heap buf (x14)
        ?ENC:encode_mov_imm64(x9, 0),         %% copy index
        {label, <<"__its_cp_", Uid/binary>>},
        ?ENC:encode_cmp_rr(x9, x13),
        ?ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, <<"__its_cpd_", Uid/binary>>, -4},
        ?ENC:encode_add_imm(x10, sp, 0),
        ?ENC:encode_add_rrr(x10, x10, x20),
        ?ENC:encode_add_rrr(x10, x10, x9),
        ?ENC:encode_ldrb(x11, x10, 0),
        ?ENC:encode_add_rrr(x12, x14, x9),
        ?ENC:encode_strb(x11, x12, 0),
        ?ENC:encode_add_imm(x9, x9, 1),
        ?ENC:encode_b(0),
        {reloc, arm64_branch26, <<"__its_cp_", Uid/binary>>, -4},
        {label, <<"__its_cpd_", Uid/binary>>},

        %% Allocate fat pointer (16 bytes) on heap
        ?ENC:encode_mov_rr(x15, x28),
        ?ENC:encode_add_imm(x28, x28, 16),    %% bump heap by 16

        %% Store {ptr, len} in fat pointer
        ?ENC:encode_str(x14, x15, 0),         %% fat.ptr = data buf
        ?ENC:encode_str(x13, x15, 8),         %% fat.len = string length

        %% Restore frame and move result to Dst
        ?ENC:encode_ldp_post(x19, x20, sp, 48),
        ?ENC:encode_mov_rr(Dst, x15)
    ]);

%% FLOAT_TO_STR: Convert float (as int64 bit pattern) to string.
%% This is a simplified implementation that converts to integer representation
%% since we don't have full FPU printf. For now, just call int_to_str on the
%% integer part of the float.
lower_instruction({float_to_str, {preg, Dst}, {preg, Src}},
                  FnName, FS, Fmt, UC) ->
    %% Convert float bit-pattern to integer first, then int_to_str
    %% FCVTZS truncates toward zero: float -> int
    SrcCode = ?ENC:reg_code(Src),
    %% FMOV D0, Xn
    FmovInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00111 bsl 16)
        bor (2#000000 bsl 10) bor (SrcCode bsl 5) bor 0,
    %% FCVTZS X16, D0
    FcvtInst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#11000 bsl 16)
        bor (2#000000 bsl 10) bor (0 bsl 5) bor ?ENC:reg_code(x16),
    FloatToInt = [<<FmovInst:32/little>>, <<FcvtInst:32/little>>],
    %% Then convert the integer part to string
    IntToStr = lower_instruction({int_to_str, {preg, Dst}, {preg, x16}},
                                  FnName, FS, Fmt, UC),
    FloatToInt ++ IntToStr;

%%====================================================================
%% Float arithmetic ARM64 lowering
%%====================================================================

%% FADD: Floating-point add. Values stored as int64 bit patterns of doubles.
%% FMOV D0, Xa; FMOV D1, Xb; FADD D0, D0, D1; FMOV Xdst, D0
lower_instruction({fadd, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    emit_float_binop(Dst, A, B, 2#0010);  %% FADD opcode bits

%% FSUB: Floating-point subtract.
lower_instruction({fsub, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    emit_float_binop(Dst, A, B, 2#0011);  %% FSUB opcode bits

%% FMUL: Floating-point multiply.
lower_instruction({fmul, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    emit_float_binop(Dst, A, B, 2#0000);  %% FMUL opcode bits

%% FDIV: Floating-point divide.
lower_instruction({fdiv, {preg, Dst}, {preg, A}, {preg, B}},
                  _FnName, _FS, _Fmt, _UC) ->
    emit_float_binop(Dst, A, B, 2#0001);  %% FDIV opcode bits

%% RAW bytes
lower_instruction({raw, Bytes}, _FnName, _FS, _Fmt, _UC) when is_binary(Bytes) ->
    [Bytes];

%% Catch-all
lower_instruction(Inst, _FnName, _FS, _Fmt, _UC) ->
    io:format(standard_error, "warning: unhandled arm64 instruction: ~p~n", [Inst]),
    [].

%%====================================================================
%% Internal helpers
%%====================================================================

%% Emit a float binary operation (fadd/fsub/fmul/fdiv).
%% Values are stored as int64 bit patterns of IEEE 754 doubles.
%% Strategy: FMOV D0, Xa; FMOV D1, Xb; FOP D0, D0, D1; FMOV Xdst, D0
%%
%% ARM64 float instructions use Vd/Vn/Vm register fields (5 bits each).
%% FMOV (general to SIMD): 1 00 11110 01 1 00111 000000 Rn Rd
%% FMOV (SIMD to general): 1 00 11110 01 1 00110 000000 Rn Rd
%% FADD: 0 00 11110 01 1 Rm 001010 Rn Rd
%% FSUB: 0 00 11110 01 1 Rm 001110 Rn Rd
%% FMUL: 0 00 11110 01 1 Rm 000010 Rn Rd
%% FDIV: 0 00 11110 01 1 Rm 000110 Rn Rd
%%
%% OpBits: FADD=0010, FSUB=0011, FMUL=0000, FDIV=0001
emit_float_binop(Dst, A, B, OpBits) ->
    ACode = ?ENC:reg_code(A),
    BCode = ?ENC:reg_code(B),
    DstCode = ?ENC:reg_code(Dst),
    %% FMOV D0, Xa (general to fp)
    FmovA = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00111 bsl 16)
        bor (2#000000 bsl 10) bor (ACode bsl 5) bor 0,
    %% FMOV D1, Xb (general to fp)
    FmovB = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00111 bsl 16)
        bor (2#000000 bsl 10) bor (BCode bsl 5) bor 1,
    %% Float op: D0 = D0 op D1
    %% Encoding: 0 00 11110 01 1 Rm OOOO10 Rn Rd
    %% where OOOO = OpBits for the operation type
    FloatOp = (2#0 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (1 bsl 16)      %% Rm=D1(=1)
        bor (OpBits bsl 12) bor (2#10 bsl 10)                   %% opcode
        bor (0 bsl 5) bor 0,                                     %% Rn=D0, Rd=D0
    %% FMOV Xdst, D0 (fp to general)
    FmovDst = (2#1 bsl 31) bor (2#00 bsl 29) bor (2#11110 bsl 24)
        bor (2#01 bsl 22) bor (2#1 bsl 21) bor (2#00110 bsl 16)
        bor (2#000000 bsl 10) bor (0 bsl 5) bor DstCode,
    [<<FmovA:32/little>>, <<FmovB:32/little>>,
     <<FloatOp:32/little>>, <<FmovDst:32/little>>].

%% Move argument values into AAPCS64 argument registers.
%% Detects cycles (e.g., x0→x1 and x1→x0) and uses x16 as temp to break them.
move_args_to_regs([], _, _) -> [];
move_args_to_regs(Args, ArgRegs, NumCalleeSaved) ->
    %% Build the full mapping first to detect cycles
    Moves = build_arg_moves_arm64(Args, ArgRegs),
    %% Detect and break cycles using x16 as scratch
    break_cycles_and_emit_arm64(Moves, NumCalleeSaved).

build_arg_moves_arm64([], _) -> [];
build_arg_moves_arm64([{preg, Reg} | Rest], [ArgReg | ArgRegs]) ->
    case Reg of
        ArgReg -> build_arg_moves_arm64(Rest, ArgRegs);
        _ -> [{ArgReg, {preg, Reg}} | build_arg_moves_arm64(Rest, ArgRegs)]
    end;
build_arg_moves_arm64([{imm, Val} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {imm, Val}} | build_arg_moves_arm64(Rest, ArgRegs)];
build_arg_moves_arm64([{stack, Slot} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {stack, Slot}} | build_arg_moves_arm64(Rest, ArgRegs)];
build_arg_moves_arm64([{vreg, _N} | Rest], [ArgReg | ArgRegs]) ->
    [{ArgReg, {imm, 0}} | build_arg_moves_arm64(Rest, ArgRegs)];
build_arg_moves_arm64(_, []) -> [].

break_cycles_and_emit_arm64(Moves, NumCalleeSaved) ->
    %% Full cycle decomposition: find strongly connected components
    %% Build adjacency map for cycle detection
    AdjMap = lists:foldl(fun({Dst, {preg, Src}}, Acc) ->
        maps:put(Dst, Src, Acc);
    (_, Acc) -> Acc end, #{}, Moves),

    %% Find all cycles
    AllCycles = find_all_cycles(AdjMap),

    case AllCycles of
        [] ->
            %% No cycles — emit directly
            [emit_move_arm64(Dst, Src, NumCalleeSaved) || {Dst, Src} <- Moves];
        _ ->
            %% Break each cycle independently using x16 as temp
            %% Process moves, breaking cycles as we go
            break_each_cycle(Moves, AllCycles, NumCalleeSaved)
    end.

%% Find all register cycles in the move graph
find_all_cycles(AdjMap) ->
    Nodes = maps:keys(AdjMap),
    find_cycles_from_nodes(Nodes, AdjMap, []).

find_cycles_from_nodes([], _AdjMap, Acc) -> Acc;
find_cycles_from_nodes([Node | Rest], AdjMap, Acc) ->
    case find_cycle_from(Node, AdjMap, [Node]) of
        false -> find_cycles_from_nodes(Rest, AdjMap, Acc);
        Cycle -> find_cycles_from_nodes(Rest, AdjMap, [Cycle | Acc])
    end.

find_cycle_from(Node, AdjMap, Path) ->
    case maps:find(Node, AdjMap) of
        error -> false;
        {ok, Next} ->
            case lists:member(Next, Path) of
                true ->
                    %% Found cycle
                    extract_cycle(Next, Path);
                false ->
                    find_cycle_from(Next, AdjMap, [Next | Path])
            end
    end.

extract_cycle(Node, Path) ->
    extract_cycle(Node, Path, []).
extract_cycle(Node, [Node | _Rest], Acc) ->
    [Node | Acc];
extract_cycle(Node, [H | Rest], Acc) ->
    extract_cycle(Node, Rest, [H | Acc]).

%% Break each cycle by saving first element to x16, emitting moves, then restoring
break_each_cycle(Moves, Cycles, NumCalleeSaved) ->
    %% For each cycle, break it by saving first reg to x16
    CycleSet = lists:flatten(Cycles),
    {CyclicMoves, AcyclicMoves} = lists:partition(
        fun({Dst, _}) -> lists:member(Dst, CycleSet) end, Moves),

    %% Emit acyclic moves first
    AcyclicCode = [emit_move_arm64(Dst, Src, NumCalleeSaved)
                   || {Dst, Src} <- AcyclicMoves],

    %% Break each cycle independently
    CyclicCode = lists:flatmap(fun(Cycle) ->
        break_single_cycle(Cycle, CyclicMoves, NumCalleeSaved)
    end, Cycles),

    AcyclicCode ++ CyclicCode.

break_single_cycle([First | _] = Cycle, AllMoves, NumCalleeSaved) ->
    %% Save first register to x16
    SaveCode = [?ENC:encode_mov_rr(x16, First)],

    %% Emit moves for this cycle (except the closing move)
    CycleMoves = [{Dst, Src} || {Dst, Src} <- AllMoves,
                                 lists:member(Dst, Cycle)],

    %% Find the move that closes the cycle (dest = First)
    {_ClosingMove, OtherMoves} = lists:partition(
        fun({Dst, _}) -> Dst =:= First end, CycleMoves),

    %% Emit other moves
    OtherCode = [emit_move_arm64(Dst, Src, NumCalleeSaved)
                 || {Dst, Src} <- OtherMoves],

    %% Close cycle by moving x16 to first destination
    CloseCode = [?ENC:encode_mov_rr(First, x16)],

    SaveCode ++ OtherCode ++ CloseCode.

emit_move_arm64(ArgReg, {preg, Reg}, _NumCalleeSaved) ->
    ?ENC:encode_mov_rr(ArgReg, Reg);
emit_move_arm64(ArgReg, {imm, Val}, _NumCalleeSaved) ->
    ?ENC:encode_mov_imm64(ArgReg, Val);
emit_move_arm64(ArgReg, {stack, Slot}, NumCalleeSaved) ->
    Offset = 16 + NumCalleeSaved * 8 + Slot * 8,
    ?ENC:encode_ldr(ArgReg, x29, Offset).
