-module(vbeam_native_arm64).

%% ARM64 (AArch64) machine code instruction encoder.
%%
%% All ARM64 instructions are exactly 32 bits (4 bytes).
%% Output as Erlang binaries in little-endian byte order.
%%
%% Register naming:
%%   x0..x30  - 64-bit general purpose registers
%%   w0..w30  - 32-bit variants (lower 32 bits)
%%   sp       - stack pointer (encoded as 31)
%%   xzr      - zero register (encoded as 31)

-export([
    %% Register utilities
    reg_code/1,

    %% Arithmetic
    encode_add_rrr/3,
    encode_add_imm/3,
    encode_sub_rrr/3,
    encode_sub_imm/3,
    encode_adds_rrr/3,
    encode_subs_rrr/3,
    encode_subs_imm/3,
    encode_mul/3,
    encode_sdiv/3,
    encode_msub/4,

    %% Logical
    encode_and_rrr/3,
    encode_orr_rrr/3,
    encode_eor_rrr/3,
    encode_ands_rrr/3,

    %% Shift
    encode_lsl_rrr/3,
    encode_lsr_rrr/3,
    encode_asr_rrr/3,

    %% Move
    encode_mov_rr/2,
    encode_movz/3,
    encode_movk/3,
    encode_mov_imm64/2,
    encode_neg/2,
    encode_mvn/2,

    %% Memory
    encode_ldr/3,
    encode_str/3,
    encode_ldrb/3,
    encode_strb/3,
    encode_ldr_literal/2,
    encode_stp/4,
    encode_ldp/4,
    encode_stp_pre/4,
    encode_ldp_post/4,
    encode_adr/2,
    encode_adrp/2,

    %% Branch
    encode_b/1,
    encode_bl/1,
    encode_b_cond/2,
    encode_br/1,
    encode_blr/1,
    encode_ret/0,
    encode_ret/1,

    %% Compare
    encode_cmp_rr/2,
    encode_cmp_imm/2,

    %% System
    encode_svc/1,
    encode_brk/1,
    encode_nop/0,

    %% Condition codes
    cond_code/1,

    %% ABI register sets
    aapcs64_arg_regs/0,
    callee_saved_regs/0,
    caller_saved_regs/0
]).

%%====================================================================
%% Register encoding
%%====================================================================

%% @doc Encode a register atom to its 5-bit numeric code (0-31).
%% sp and xzr both encode as 31; context determines interpretation.
-spec reg_code(atom()) -> 0..31.
reg_code(x0)  -> 0;
reg_code(x1)  -> 1;
reg_code(x2)  -> 2;
reg_code(x3)  -> 3;
reg_code(x4)  -> 4;
reg_code(x5)  -> 5;
reg_code(x6)  -> 6;
reg_code(x7)  -> 7;
reg_code(x8)  -> 8;
reg_code(x9)  -> 9;
reg_code(x10) -> 10;
reg_code(x11) -> 11;
reg_code(x12) -> 12;
reg_code(x13) -> 13;
reg_code(x14) -> 14;
reg_code(x15) -> 15;
reg_code(x16) -> 16;
reg_code(x17) -> 17;
reg_code(x18) -> 18;
reg_code(x19) -> 19;
reg_code(x20) -> 20;
reg_code(x21) -> 21;
reg_code(x22) -> 22;
reg_code(x23) -> 23;
reg_code(x24) -> 24;
reg_code(x25) -> 25;
reg_code(x26) -> 26;
reg_code(x27) -> 27;
reg_code(x28) -> 28;
reg_code(x29) -> 29;  %% frame pointer
reg_code(x30) -> 30;  %% link register
reg_code(sp)  -> 31;
reg_code(xzr) -> 31;
%% 32-bit aliases
reg_code(w0)  -> 0;
reg_code(w1)  -> 1;
reg_code(w2)  -> 2;
reg_code(w3)  -> 3;
reg_code(w4)  -> 4;
reg_code(w5)  -> 5;
reg_code(w6)  -> 6;
reg_code(w7)  -> 7;
reg_code(w8)  -> 8;
reg_code(w9)  -> 9;
reg_code(w10) -> 10;
reg_code(w11) -> 11;
reg_code(w12) -> 12;
reg_code(w13) -> 13;
reg_code(w14) -> 14;
reg_code(w15) -> 15;
reg_code(w16) -> 16;
reg_code(w17) -> 17;
reg_code(w18) -> 18;
reg_code(w19) -> 19;
reg_code(w20) -> 20;
reg_code(w21) -> 21;
reg_code(w22) -> 22;
reg_code(w23) -> 23;
reg_code(w24) -> 24;
reg_code(w25) -> 25;
reg_code(w26) -> 26;
reg_code(w27) -> 27;
reg_code(w28) -> 28;
reg_code(w29) -> 29;
reg_code(w30) -> 30.

%%====================================================================
%% Condition codes
%%====================================================================

%% @doc Convert a condition atom to its 4-bit encoding.
-spec cond_code(atom()) -> 0..15.
cond_code(eq)  -> 16#0;  %% Equal (Z=1)
cond_code(ne)  -> 16#1;  %% Not equal (Z=0)
cond_code(cs)  -> 16#2;  %% Carry set / unsigned >=
cond_code(geu) -> 16#2;  %% alias for CS
cond_code(hs)  -> 16#2;  %% alias for CS (higher-or-same)
cond_code(cc)  -> 16#3;  %% Carry clear / unsigned <
cond_code(ltu) -> 16#3;  %% alias for CC
cond_code(lo)  -> 16#3;  %% alias for CC (lower)
cond_code(mi)  -> 16#4;  %% Minus / negative
cond_code(pl)  -> 16#5;  %% Plus / positive or zero
cond_code(vs)  -> 16#6;  %% Overflow
cond_code(vc)  -> 16#7;  %% No overflow
cond_code(hi)  -> 16#8;  %% Unsigned >
cond_code(gtu) -> 16#8;  %% alias for HI
cond_code(ls)  -> 16#9;  %% Unsigned <=
cond_code(leu) -> 16#9;  %% alias for LS
cond_code(ge)  -> 16#A;  %% Signed >=
cond_code(lt)  -> 16#B;  %% Signed <
cond_code(gt)  -> 16#C;  %% Signed >
cond_code(le)  -> 16#D;  %% Signed <=
cond_code(al)  -> 16#E.  %% Always

%%====================================================================
%% ABI register sets (AAPCS64)
%%====================================================================

%% @doc Argument registers for AAPCS64 calling convention.
-spec aapcs64_arg_regs() -> [atom()].
aapcs64_arg_regs() ->
    [x0, x1, x2, x3, x4, x5, x6, x7].

%% @doc Callee-saved registers (must be preserved across calls).
-spec callee_saved_regs() -> [atom()].
callee_saved_regs() ->
    [x19, x20, x21, x22, x23, x24, x25, x26, x27, x28].

%% @doc Caller-saved registers (may be clobbered by calls).
%% Includes x0-x18 and x30 (link register).
-spec caller_saved_regs() -> [atom()].
caller_saved_regs() ->
    [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9,
     x10, x11, x12, x13, x14, x15, x16, x17, x18, x30].

%%====================================================================
%% Internal helpers
%%====================================================================

%% Emit a 32-bit instruction as a 4-byte little-endian binary.
-spec emit(non_neg_integer()) -> binary().
emit(Instruction) ->
    <<Instruction:32/little>>.

%%====================================================================
%% Arithmetic instructions (64-bit, sf=1)
%%====================================================================

%% @doc ADD Xd, Xn, Xm — add two registers.
%% Encoding: sf=1, opc=00, S=0, 01011 shift=00 0 Rm imm6=000000 Rn Rd
%% Bit layout: [1][00][0][1011][00][0][Rm:5][000000][Rn:5][Rd:5]
-spec encode_add_rrr(atom(), atom(), atom()) -> binary().
encode_add_rrr(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1 (64-bit)
        bor (2#00 bsl 29)                    %% opc=00 (ADD)
        bor (2#0 bsl 28)                     %% S=0 (no flags)
        bor (2#01011 bsl 24)                 %% fixed
        bor (2#00 bsl 22)                    %% shift=LSL
        bor (2#0 bsl 21)                     %% fixed
        bor (reg_code(Rm) bsl 16)
        bor (2#000000 bsl 10)                %% imm6=0
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc ADD Xd, Xn, #imm12 — add immediate.
%% Encoding: sf=1, opc=00, S=0, 100010 sh=0 imm12 Rn Rd
-spec encode_add_imm(atom(), atom(), non_neg_integer()) -> binary().
encode_add_imm(Rd, Rn, Imm12) when Imm12 >= 0, Imm12 < 4096 ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#00 bsl 29)                    %% opc=00 (ADD)
        bor (2#0 bsl 28)                     %% S=0
        bor (2#100010 bsl 23)                %% fixed (shifted imm)
        bor (2#0 bsl 22)                     %% sh=0 (no shift)
        bor (Imm12 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc SUB Xd, Xn, Xm — subtract registers.
%% Same as ADD but opc=10.
-spec encode_sub_rrr(atom(), atom(), atom()) -> binary().
encode_sub_rrr(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#10 bsl 29)                    %% opc=10 (SUB)
        bor (2#0 bsl 28)                     %% S=0
        bor (2#01011 bsl 24)
        bor (2#00 bsl 22)
        bor (2#0 bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (2#000000 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc SUB Xd, Xn, #imm12 — subtract immediate.
-spec encode_sub_imm(atom(), atom(), non_neg_integer()) -> binary().
encode_sub_imm(Rd, Rn, Imm12) when Imm12 >= 0, Imm12 < 4096 ->
    Inst = (2#1 bsl 31)
        bor (2#10 bsl 29)                    %% opc=10 (SUB)
        bor (2#0 bsl 28)                     %% S=0
        bor (2#100010 bsl 23)
        bor (2#0 bsl 22)
        bor (Imm12 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc ADDS Xd, Xn, Xm — add registers, setting flags.
-spec encode_adds_rrr(atom(), atom(), atom()) -> binary().
encode_adds_rrr(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#01 bsl 29)                    %% opc=01 (ADDS: opc=00,S=1 but encoded as 01+S... )
        bor (2#01011 bsl 24)
        bor (2#00 bsl 22)
        bor (2#0 bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (2#000000 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc SUBS Xd, Xn, Xm — subtract registers, setting flags.
%% CMP Xn, Xm is an alias for SUBS XZR, Xn, Xm.
-spec encode_subs_rrr(atom(), atom(), atom()) -> binary().
encode_subs_rrr(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#11 bsl 29)                    %% opc=11 (SUBS: opc=10,S=1 -> bits 29..28 = 11)
        bor (2#01011 bsl 24)
        bor (2#00 bsl 22)
        bor (2#0 bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (2#000000 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc SUBS Xd, Xn, #imm12 — subtract immediate, setting flags.
%% CMP Xn, #imm12 when Rd=xzr.
-spec encode_subs_imm(atom(), atom(), non_neg_integer()) -> binary().
encode_subs_imm(Rd, Rn, Imm12) when Imm12 >= 0, Imm12 < 4096 ->
    Inst = (2#1 bsl 31)
        bor (2#11 bsl 29)                    %% SUBS
        bor (2#100010 bsl 23)
        bor (2#0 bsl 22)
        bor (Imm12 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc MUL Xd, Xn, Xm — multiply (alias for MADD Xd, Xn, Xm, XZR).
%% Encoding: sf=1, 00 11011 000 Rm 0 Ra=11111 Rn Rd
%% Data Processing (3 source): [sf:1][00][11011][000][Rm:5][0][Ra:5][Rn:5][Rd:5]
-spec encode_mul(atom(), atom(), atom()) -> binary().
encode_mul(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#00 bsl 29)                    %% op54=00
        bor (2#11011 bsl 24)                 %% fixed
        bor (2#000 bsl 21)                   %% op31=000 (MADD)
        bor (reg_code(Rm) bsl 16)
        bor (2#0 bsl 15)                     %% o0=0 (MADD, not MSUB)
        bor (31 bsl 10)                      %% Ra=XZR (makes it MUL)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc SDIV Xd, Xn, Xm — signed divide.
%% Encoding: sf=1, 0 0 11010110 Rm 00001 1 Rn Rd
%% Data Processing (2 source): [sf:1][0][S:1][11010110][Rm:5][opcode:6][Rn:5][Rd:5]
%% SDIV opcode = 000011
-spec encode_sdiv(atom(), atom(), atom()) -> binary().
encode_sdiv(Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#0 bsl 30)                     %% op=0
        bor (2#0 bsl 29)                     %% S=0
        bor (2#11010110 bsl 21)              %% fixed
        bor (reg_code(Rm) bsl 16)
        bor (2#000011 bsl 10)                %% opcode=000011 (SDIV)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc MSUB Xd, Xn, Xm, Xa — multiply-subtract: Xd = Xa - Xn*Xm.
%% Useful for remainder: rem = a - (a/b)*b.
%% Encoding: sf=1, 00 11011 000 Rm 1 Ra Rn Rd
-spec encode_msub(atom(), atom(), atom(), atom()) -> binary().
encode_msub(Rd, Rn, Rm, Ra) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#00 bsl 29)
        bor (2#11011 bsl 24)
        bor (2#000 bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (2#1 bsl 15)                     %% o0=1 (MSUB, not MADD)
        bor (reg_code(Ra) bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%%====================================================================
%% Logical instructions (64-bit, sf=1)
%%====================================================================

%% Logical (shifted register) encoding:
%% [sf:1][opc:2][01010][shift:2][N:1][Rm:5][imm6:6][Rn:5][Rd:5]
%% AND: opc=00, N=0
%% ORR: opc=01, N=0
%% EOR: opc=10, N=0
%% ANDS: opc=11, N=0
%% ORN: opc=01, N=1

%% @doc AND Xd, Xn, Xm — bitwise AND.
-spec encode_and_rrr(atom(), atom(), atom()) -> binary().
encode_and_rrr(Rd, Rn, Rm) ->
    encode_logical_shifted(2#00, 0, Rd, Rn, Rm).

%% @doc ORR Xd, Xn, Xm — bitwise OR.
-spec encode_orr_rrr(atom(), atom(), atom()) -> binary().
encode_orr_rrr(Rd, Rn, Rm) ->
    encode_logical_shifted(2#01, 0, Rd, Rn, Rm).

%% @doc EOR Xd, Xn, Xm — bitwise exclusive OR.
-spec encode_eor_rrr(atom(), atom(), atom()) -> binary().
encode_eor_rrr(Rd, Rn, Rm) ->
    encode_logical_shifted(2#10, 0, Rd, Rn, Rm).

%% @doc ANDS Xd, Xn, Xm — bitwise AND, setting flags.
%% TST Xn, Xm is an alias for ANDS XZR, Xn, Xm.
-spec encode_ands_rrr(atom(), atom(), atom()) -> binary().
encode_ands_rrr(Rd, Rn, Rm) ->
    encode_logical_shifted(2#11, 0, Rd, Rn, Rm).

%% Internal: encode a logical (shifted register) instruction.
-spec encode_logical_shifted(0..3, 0..1, atom(), atom(), atom()) -> binary().
encode_logical_shifted(Opc, N, Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (Opc bsl 29)
        bor (2#01010 bsl 24)                 %% fixed
        bor (2#00 bsl 22)                    %% shift=LSL
        bor (N bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (2#000000 bsl 10)                %% imm6=0 (no shift amount)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%%====================================================================
%% Shift instructions (64-bit)
%%====================================================================

%% Variable shifts use Data Processing (2 source) encoding:
%% [sf:1][0][S:1][11010110][Rm:5][opcode:6][Rn:5][Rd:5]
%% LSLV: opcode = 001000
%% LSRV: opcode = 001001
%% ASRV: opcode = 001010

%% @doc LSL Xd, Xn, Xm — logical shift left (variable).
-spec encode_lsl_rrr(atom(), atom(), atom()) -> binary().
encode_lsl_rrr(Rd, Rn, Rm) ->
    encode_shift_var(2#001000, Rd, Rn, Rm).

%% @doc LSR Xd, Xn, Xm — logical shift right (variable).
-spec encode_lsr_rrr(atom(), atom(), atom()) -> binary().
encode_lsr_rrr(Rd, Rn, Rm) ->
    encode_shift_var(2#001001, Rd, Rn, Rm).

%% @doc ASR Xd, Xn, Xm — arithmetic shift right (variable).
-spec encode_asr_rrr(atom(), atom(), atom()) -> binary().
encode_asr_rrr(Rd, Rn, Rm) ->
    encode_shift_var(2#001010, Rd, Rn, Rm).

%% Internal: encode a variable shift instruction.
-spec encode_shift_var(non_neg_integer(), atom(), atom(), atom()) -> binary().
encode_shift_var(Opcode, Rd, Rn, Rm) ->
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#0 bsl 30)
        bor (2#0 bsl 29)                     %% S=0
        bor (2#11010110 bsl 21)
        bor (reg_code(Rm) bsl 16)
        bor (Opcode bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%%====================================================================
%% Move instructions
%%====================================================================

%% @doc MOV Xd, Xn — move register.
%% SP cannot be used in ORR; MOV involving SP uses ADD Xd, Xn, #0.
-spec encode_mov_rr(atom(), atom()) -> binary().
encode_mov_rr(Rd, sp) ->
    encode_add_imm(Rd, sp, 0);
encode_mov_rr(sp, Rn) ->
    encode_add_imm(sp, Rn, 0);
encode_mov_rr(Rd, Rn) ->
    encode_orr_rrr(Rd, xzr, Rn).

%% @doc MOVZ Xd, #imm16, LSL #shift — move wide with zero.
%% Clears other bits, places imm16 at the shifted position.
%% Shift must be 0, 16, 32, or 48.
%% Encoding: [sf:1][opc:2][100101][hw:2][imm16:16][Rd:5]
%% MOVZ: opc=10
-spec encode_movz(atom(), non_neg_integer(), non_neg_integer()) -> binary().
encode_movz(Rd, Imm16, Shift) when Imm16 >= 0, Imm16 < 65536 ->
    Hw = Shift div 16,
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#10 bsl 29)                    %% opc=10 (MOVZ)
        bor (2#100101 bsl 23)                %% fixed
        bor (Hw bsl 21)
        bor (Imm16 bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc MOVK Xd, #imm16, LSL #shift — move wide with keep.
%% Inserts imm16 at the shifted position, keeps other bits.
%% MOVK: opc=11
-spec encode_movk(atom(), non_neg_integer(), non_neg_integer()) -> binary().
encode_movk(Rd, Imm16, Shift) when Imm16 >= 0, Imm16 < 65536 ->
    Hw = Shift div 16,
    Inst = (2#1 bsl 31)                      %% sf=1
        bor (2#11 bsl 29)                    %% opc=11 (MOVK)
        bor (2#100101 bsl 23)
        bor (Hw bsl 21)
        bor (Imm16 bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc Load a full 64-bit immediate into Rd using MOVZ + up to 3 MOVK.
%% Emits the minimal sequence needed.
-spec encode_mov_imm64(atom(), integer()) -> binary().
encode_mov_imm64(Rd, Value) when Value < 0 ->
    %% Convert negative to unsigned 64-bit two's complement
    encode_mov_imm64(Rd, Value band 16#FFFFFFFFFFFFFFFF);
encode_mov_imm64(Rd, Value) when Value >= 0 ->
    %% Extract 16-bit chunks
    W0 = Value band 16#FFFF,
    W1 = (Value bsr 16) band 16#FFFF,
    W2 = (Value bsr 32) band 16#FFFF,
    W3 = (Value bsr 48) band 16#FFFF,
    %% Always start with MOVZ for the lowest non-zero chunk (or chunk 0)
    {StartShift, Chunks} = find_first_nonzero([{0, W0}, {16, W1}, {32, W2}, {48, W3}]),
    First = encode_movz(Rd, element(2, StartShift), element(1, StartShift)),
    Rest = lists:foldl(
        fun({Shift, Word}, Acc) ->
            case Word of
                0 -> Acc;
                _ -> <<Acc/binary, (encode_movk(Rd, Word, Shift))/binary>>
            end
        end,
        First,
        Chunks
    ),
    Rest.

%% Find the first non-zero chunk for MOVZ, return it and the remaining chunks for MOVK.
-spec find_first_nonzero([{non_neg_integer(), non_neg_integer()}]) ->
    {{non_neg_integer(), non_neg_integer()}, [{non_neg_integer(), non_neg_integer()}]}.
find_first_nonzero([]) ->
    %% Value is 0: MOVZ with 0
    {{0, 0}, []};
find_first_nonzero([{Shift, Word} | Rest]) when Word =/= 0 ->
    {{Shift, Word}, Rest};
find_first_nonzero([_ | Rest]) ->
    find_first_nonzero(Rest).

%% @doc NEG Xd, Xm — negate (alias for SUB Xd, XZR, Xm).
-spec encode_neg(atom(), atom()) -> binary().
encode_neg(Rd, Rm) ->
    encode_sub_rrr(Rd, xzr, Rm).

%% @doc MVN Xd, Xm — bitwise NOT (alias for ORN Xd, XZR, Xm).
%% ORN encoding: opc=01, N=1
-spec encode_mvn(atom(), atom()) -> binary().
encode_mvn(Rd, Rm) ->
    encode_logical_shifted(2#01, 1, Rd, xzr, Rm).

%%====================================================================
%% Memory instructions (64-bit)
%%====================================================================

%% @doc LDR Xt, [Xn, #offset] — load 64-bit from base+offset.
%% Unsigned offset encoding: [size:2][111001][opc:2][imm12:12][Rn:5][Rt:5]
%% 64-bit LDR: size=11, opc=01
%% Offset is in bytes, must be 8-byte aligned; encoded as offset/8.
-spec encode_ldr(atom(), atom(), non_neg_integer()) -> binary().
encode_ldr(Rt, Rn, Offset) when Offset >= 0, Offset rem 8 =:= 0 ->
    ScaledOff = Offset div 8,
    true = (ScaledOff < 4096),
    Inst = (2#11 bsl 30)                     %% size=11 (64-bit)
        bor (2#111001 bsl 24)                %% fixed (load/store unsigned offset)
        bor (2#01 bsl 22)                    %% opc=01 (LDR)
        bor (ScaledOff bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc STR Xt, [Xn, #offset] — store 64-bit to base+offset.
%% 64-bit STR: size=11, opc=00
-spec encode_str(atom(), atom(), non_neg_integer()) -> binary().
encode_str(Rt, Rn, Offset) when Offset >= 0, Offset rem 8 =:= 0 ->
    ScaledOff = Offset div 8,
    true = (ScaledOff < 4096),
    Inst = (2#11 bsl 30)                     %% size=11 (64-bit)
        bor (2#111001 bsl 24)
        bor (2#00 bsl 22)                    %% opc=00 (STR)
        bor (ScaledOff bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc STRB Wt, [Xn, #offset] — store byte to base+offset.
%% Byte store: size=00, opc=00. Offset is NOT scaled (byte offset).
-spec encode_strb(atom(), atom(), non_neg_integer()) -> binary().
encode_strb(Rt, Rn, Offset) when Offset >= 0, Offset < 4096 ->
    Inst = (2#00 bsl 30)                     %% size=00 (byte)
        bor (2#111001 bsl 24)                %% fixed (load/store unsigned offset)
        bor (2#00 bsl 22)                    %% opc=00 (STR)
        bor (Offset bsl 10)                  %% imm12 (unscaled for byte)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc LDRB Wt, [Xn, #offset] — load byte from base+offset.
%% Byte load: size=00, opc=01.
-spec encode_ldrb(atom(), atom(), non_neg_integer()) -> binary().
encode_ldrb(Rt, Rn, Offset) when Offset >= 0, Offset < 4096 ->
    Inst = (2#00 bsl 30)                     %% size=00 (byte)
        bor (2#111001 bsl 24)
        bor (2#01 bsl 22)                    %% opc=01 (LDR)
        bor (Offset bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc LDR Xt, label — PC-relative literal load.
%% Encoding: [opc:2][011000][imm19:19][Rt:5]
%% 64-bit: opc=01. imm19 is signed offset in instructions (offset/4).
-spec encode_ldr_literal(atom(), integer()) -> binary().
encode_ldr_literal(Rt, Offset) when Offset rem 4 =:= 0 ->
    InsnOffset = Offset div 4,
    %% Validate signed 19-bit range: [-2^18, 2^18-1]
    case (InsnOffset >= -(1 bsl 18)) andalso (InsnOffset < (1 bsl 18)) of
        true -> ok;
        false -> error({arm64_ldr_literal_imm19_overflow, Offset})
    end,
    Imm19 = InsnOffset band 16#7FFFF,
    Inst = (2#01 bsl 30)                     %% opc=01 (64-bit)
        bor (2#011000 bsl 24)                %% fixed
        bor (Imm19 bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc STP Xt, Xt2, [Xn, #offset] — store pair, signed offset (no writeback).
%% Encoding: [opc:2][101][00][10][L:1][imm7:7][Rt2:5][Rn:5][Rt:5]
%% 64-bit: opc=10, L=0 (store). Offset mode: bits 24:23 = 10.
%% Offset is signed, scaled by 8: imm7 = offset/8.
-spec encode_stp(atom(), atom(), atom(), integer()) -> binary().
encode_stp(Rt, Rt2, Rn, Offset) when Offset rem 8 =:= 0 ->
    ScaledOffset = Offset div 8,
    %% Validate signed 7-bit range: [-64, 63]
    %% In bytes (scaled by 8): [-512, 504]
    case (ScaledOffset >= -(1 bsl 6)) andalso (ScaledOffset < (1 bsl 6)) of
        true -> ok;
        false -> error({arm64_stp_imm7_overflow, Offset})
    end,
    Imm7 = ScaledOffset band 16#7F,
    Inst = (2#10 bsl 30)                     %% opc=10 (64-bit)
        bor (2#101 bsl 27)                   %% fixed
        bor (2#00 bsl 25)                    %% fixed
        bor (2#10 bsl 23)                    %% signed offset mode (no writeback)
        bor (2#0 bsl 22)                     %% L=0 (store)
        bor (Imm7 bsl 15)
        bor (reg_code(Rt2) bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc STP Xt, Xt2, [Xn, #offset]! — store pair, pre-index.
%% Encoding: [opc:2][101][00][11][L:1][imm7:7][Rt2:5][Rn:5][Rt:5]
%% 64-bit: opc=10, L=0 (store). Pre-index variant: bits 24:23 = 11.
%% Offset is signed, scaled by 8: imm7 = offset/8.
-spec encode_stp_pre(atom(), atom(), atom(), integer()) -> binary().
encode_stp_pre(Rt, Rt2, Rn, Offset) when Offset rem 8 =:= 0 ->
    ScaledOffset = Offset div 8,
    %% Validate signed 7-bit range: [-64, 63]
    %% In bytes (scaled by 8): [-512, 504]
    case (ScaledOffset >= -(1 bsl 6)) andalso (ScaledOffset < (1 bsl 6)) of
        true -> ok;
        false -> error({arm64_stp_imm7_overflow, Offset})
    end,
    Imm7 = ScaledOffset band 16#7F,
    Inst = (2#10 bsl 30)                     %% opc=10 (64-bit)
        bor (2#101 bsl 27)                   %% fixed
        bor (2#00 bsl 25)                    %% fixed
        bor (2#11 bsl 23)                    %% pre-index
        bor (2#0 bsl 22)                     %% L=0 (store)
        bor (Imm7 bsl 15)
        bor (reg_code(Rt2) bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc LDP Xt, Xt2, [Xn, #offset] — load pair, signed offset (no writeback).
%% Encoding: [opc:2][101][00][10][L:1][imm7:7][Rt2:5][Rn:5][Rt:5]
%% 64-bit: opc=10, L=1 (load). Offset mode: bits 24:23 = 10.
-spec encode_ldp(atom(), atom(), atom(), integer()) -> binary().
encode_ldp(Rt, Rt2, Rn, Offset) when Offset rem 8 =:= 0 ->
    ScaledOffset = Offset div 8,
    %% Validate signed 7-bit range: [-64, 63]
    %% In bytes (scaled by 8): [-512, 504]
    case (ScaledOffset >= -(1 bsl 6)) andalso (ScaledOffset < (1 bsl 6)) of
        true -> ok;
        false -> error({arm64_ldp_imm7_overflow, Offset})
    end,
    Imm7 = ScaledOffset band 16#7F,
    Inst = (2#10 bsl 30)                     %% opc=10 (64-bit)
        bor (2#101 bsl 27)
        bor (2#00 bsl 25)
        bor (2#10 bsl 23)                    %% signed offset mode (no writeback)
        bor (2#1 bsl 22)                     %% L=1 (load)
        bor (Imm7 bsl 15)
        bor (reg_code(Rt2) bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc LDP Xt, Xt2, [Xn], #offset — load pair, post-index.
%% 64-bit: opc=10, L=1 (load). Post-index variant: bits 24:23 = 01.
-spec encode_ldp_post(atom(), atom(), atom(), integer()) -> binary().
encode_ldp_post(Rt, Rt2, Rn, Offset) when Offset rem 8 =:= 0 ->
    ScaledOffset = Offset div 8,
    %% Validate signed 7-bit range: [-64, 63]
    %% In bytes (scaled by 8): [-512, 504]
    case (ScaledOffset >= -(1 bsl 6)) andalso (ScaledOffset < (1 bsl 6)) of
        true -> ok;
        false -> error({arm64_ldp_imm7_overflow, Offset})
    end,
    Imm7 = ScaledOffset band 16#7F,
    Inst = (2#10 bsl 30)                     %% opc=10 (64-bit)
        bor (2#101 bsl 27)
        bor (2#00 bsl 25)
        bor (2#01 bsl 23)                    %% post-index
        bor (2#1 bsl 22)                     %% L=1 (load)
        bor (Imm7 bsl 15)
        bor (reg_code(Rt2) bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor reg_code(Rt),
    emit(Inst).

%% @doc ADR Xd, label — form PC-relative address (+-1MB).
%% Encoding: [immlo:2][10000][immhi:19][Rd:5]
%% offset = immhi:immlo (21-bit signed), in bytes.
-spec encode_adr(atom(), integer()) -> binary().
encode_adr(Rd, Offset) ->
    %% Validate signed 21-bit range: [-2^20, 2^20-1]
    case (Offset >= -(1 bsl 20)) andalso (Offset < (1 bsl 20)) of
        true -> ok;
        false -> error({arm64_adr_imm21_overflow, Offset})
    end,
    Imm21 = Offset band 16#1FFFFF,
    ImmLo = Imm21 band 2#11,
    ImmHi = (Imm21 bsr 2) band 16#7FFFF,
    Inst = (ImmLo bsl 29)
        bor (2#10000 bsl 24)                 %% op=0 (ADR)
        bor (ImmHi bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%% @doc ADRP Xd, label — form PC-relative page address (+-4GB).
%% Same encoding as ADR but op=1 (bit 31).
%% Offset is page-aligned (4KB granularity): immhi:immlo << 12.
-spec encode_adrp(atom(), integer()) -> binary().
encode_adrp(Rd, Offset) ->
    %% Offset is in pages (already divided by 4096 by caller,
    %% or we divide here). We take the raw page offset.
    %% Validate signed 21-bit range: [-2^20, 2^20-1]
    case (Offset >= -(1 bsl 20)) andalso (Offset < (1 bsl 20)) of
        true -> ok;
        false -> error({arm64_adrp_imm21_overflow, Offset})
    end,
    Imm21 = Offset band 16#1FFFFF,
    ImmLo = Imm21 band 2#11,
    ImmHi = (Imm21 bsr 2) band 16#7FFFF,
    Inst = (2#1 bsl 31)                      %% op=1 (ADRP)
        bor (ImmLo bsl 29)
        bor (2#10000 bsl 24)
        bor (ImmHi bsl 5)
        bor reg_code(Rd),
    emit(Inst).

%%====================================================================
%% Branch instructions
%%====================================================================

%% @doc B label — unconditional branch (PC-relative, +-128MB).
%% Encoding: [000101][imm26:26]
%% imm26 is signed offset in instructions (offset/4).
-spec encode_b(integer()) -> binary().
encode_b(Offset) when Offset rem 4 =:= 0 ->
    InsnOffset = Offset div 4,
    %% Validate signed 26-bit range: [-2^25, 2^25-1]
    case (InsnOffset >= -(1 bsl 25)) andalso (InsnOffset < (1 bsl 25)) of
        true -> ok;
        false -> error({arm64_b_imm26_overflow, Offset})
    end,
    Imm26 = InsnOffset band 16#3FFFFFF,
    Inst = (2#000101 bsl 26)
        bor Imm26,
    emit(Inst).

%% @doc BL label — branch with link (call, PC-relative, +-128MB).
%% Encoding: [100101][imm26:26]
-spec encode_bl(integer()) -> binary().
encode_bl(Offset) when Offset rem 4 =:= 0 ->
    InsnOffset = Offset div 4,
    %% Validate signed 26-bit range: [-2^25, 2^25-1]
    case (InsnOffset >= -(1 bsl 25)) andalso (InsnOffset < (1 bsl 25)) of
        true -> ok;
        false -> error({arm64_bl_imm26_overflow, Offset})
    end,
    Imm26 = InsnOffset band 16#3FFFFFF,
    Inst = (2#100101 bsl 26)
        bor Imm26,
    emit(Inst).

%% @doc B.cond label — conditional branch.
%% Encoding: [01010100][imm19:19][0:1][cond:4]
%% imm19 is signed offset in instructions (offset/4).
-spec encode_b_cond(atom(), integer()) -> binary().
encode_b_cond(Cond, Offset) when Offset rem 4 =:= 0 ->
    InsnOffset = Offset div 4,
    %% Validate signed 19-bit range: [-2^18, 2^18-1]
    case (InsnOffset >= -(1 bsl 18)) andalso (InsnOffset < (1 bsl 18)) of
        true -> ok;
        false -> error({arm64_b_cond_imm19_overflow, Offset})
    end,
    Imm19 = InsnOffset band 16#7FFFF,
    CondBits = cond_code(Cond),
    Inst = (2#01010100 bsl 24)
        bor (Imm19 bsl 5)
        bor (2#0 bsl 4)                      %% fixed bit 4 = 0
        bor CondBits,
    emit(Inst).

%% @doc BR Xn — branch to register (indirect branch).
%% Encoding: [1101011][0][0][00][11111][000000][Rn:5][00000]
-spec encode_br(atom()) -> binary().
encode_br(Rn) ->
    Inst = (2#1101011 bsl 25)
        bor (2#0 bsl 24)                     %% Z=0
        bor (2#0 bsl 23)                     %% op=0
        bor (2#00 bsl 21)                    %% op2=00
        bor (2#11111 bsl 16)                 %% fixed
        bor (2#000000 bsl 10)                %% fixed
        bor (reg_code(Rn) bsl 5)
        bor 2#00000,                         %% Rm=0
    emit(Inst).

%% @doc BLR Xn — branch with link to register (indirect call).
%% Encoding: [1101011][0][0][01][11111][000000][Rn:5][00000]
-spec encode_blr(atom()) -> binary().
encode_blr(Rn) ->
    Inst = (2#1101011 bsl 25)
        bor (2#0 bsl 24)
        bor (2#0 bsl 23)
        bor (2#01 bsl 21)                    %% op2=01 (BLR)
        bor (2#11111 bsl 16)
        bor (2#000000 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor 2#00000,
    emit(Inst).

%% @doc RET Xn — return (branch to Xn, typically x30).
%% Encoding: [1101011][0][0][10][11111][000000][Rn:5][00000]
-spec encode_ret(atom()) -> binary().
encode_ret(Rn) ->
    Inst = (2#1101011 bsl 25)
        bor (2#0 bsl 24)
        bor (2#0 bsl 23)
        bor (2#10 bsl 21)                    %% op2=10 (RET)
        bor (2#11111 bsl 16)
        bor (2#000000 bsl 10)
        bor (reg_code(Rn) bsl 5)
        bor 2#00000,
    emit(Inst).

%% @doc RET — return via x30 (link register).
-spec encode_ret() -> binary().
encode_ret() ->
    encode_ret(x30).

%%====================================================================
%% Compare instructions
%%====================================================================

%% @doc CMP Xn, Xm — compare registers (alias for SUBS XZR, Xn, Xm).
-spec encode_cmp_rr(atom(), atom()) -> binary().
encode_cmp_rr(Rn, Rm) ->
    encode_subs_rrr(xzr, Rn, Rm).

%% @doc CMP Xn, #imm12 — compare with immediate (alias for SUBS XZR, Xn, #imm12).
-spec encode_cmp_imm(atom(), non_neg_integer()) -> binary().
encode_cmp_imm(Rn, Imm12) ->
    encode_subs_imm(xzr, Rn, Imm12).

%%====================================================================
%% System instructions
%%====================================================================

%% @doc SVC #imm16 — supervisor call (syscall).
%% Encoding: [11010100][000][imm16:16][000][01]
-spec encode_svc(non_neg_integer()) -> binary().
encode_svc(Imm16) when Imm16 >= 0, Imm16 < 65536 ->
    Inst = (2#11010100 bsl 24)
        bor (2#000 bsl 21)                   %% opc=000
        bor (Imm16 bsl 5)
        bor (2#000 bsl 2)                    %% op2=000
        bor 2#01,                            %% LL=01 (SVC)
    emit(Inst).

%% @doc BRK #imm16 — software breakpoint (debugger trap).
%% Encoding: 0xD4200000 | (imm16 << 5)
-spec encode_brk(non_neg_integer()) -> binary().
encode_brk(Imm16) when Imm16 >= 0, Imm16 < 65536 ->
    Inst = 16#D4200000 bor (Imm16 bsl 5),
    emit(Inst).

%% @doc NOP — no operation.
%% Encoding: 0xD503201F
-spec encode_nop() -> binary().
encode_nop() ->
    emit(16#D503201F).
