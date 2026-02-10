-module(vbeam_native_x86_64).

%% x86_64 machine code instruction encoder.
%%
%% Emits raw bytes as Erlang binaries for x86_64 instructions.
%% Uses Intel SDM vol 2 encodings. All 64-bit operand size by default
%% (REX.W prefix).
%%
%% Register encoding:
%%   rax=0, rcx=1, rdx=2, rbx=3, rsp=4, rbp=5, rsi=6, rdi=7
%%   r8=8,  r9=9,  r10=10, r11=11, r12=12, r13=13, r14=14, r15=15
%%
%% REX prefix: 0100WRXB
%%   W=1 for 64-bit operand size
%%   R=1 extends ModR/M reg field
%%   X=1 extends SIB index field
%%   B=1 extends ModR/M r/m or SIB base field
%%
%% ModR/M byte: [Mod:2][Reg:3][R/M:3]
%%   Mod=00: [r/m], Mod=01: [r/m+disp8], Mod=10: [r/m+disp32], Mod=11: r/m
%%   r/m=4 (rsp) requires SIB byte
%%   r/m=5 (rbp) with Mod=00 means [RIP+disp32]

-export([
    %% Register helpers
    reg_code/1,
    reg_lo/1,
    reg_hi/1,
    needs_rex/1,

    %% Encoding helpers
    rex/4,
    rex_w/1,
    modrm/3,

    %% Arithmetic: mov
    encode_mov_rr/2,
    encode_mov_imm64/2,
    encode_mov_imm32/2,

    %% Arithmetic: add, sub
    encode_add_rr/2,
    encode_add_imm/2,
    encode_sub_rr/2,
    encode_sub_imm/2,

    %% Arithmetic: mul, div
    encode_imul_rr/2,
    encode_idiv/1,
    encode_cqo/0,

    %% Bitwise
    encode_and_rr/2,
    encode_or_rr/2,
    encode_xor_rr/2,
    encode_shl_cl/1,
    encode_shr_cl/1,
    encode_sar_cl/1,
    encode_neg/1,
    encode_not/1,

    %% Compare / test
    encode_cmp_rr/2,
    encode_cmp_imm/2,
    encode_test_rr/2,

    %% Control flow
    encode_jmp_rel32/1,
    encode_jcc_rel32/2,
    encode_jns/1,
    encode_jle/1,
    encode_call_rel32/1,
    encode_call_reg/1,
    encode_ret/0,

    %% Stack
    encode_push/1,
    encode_push_imm32/1,
    encode_pop/1,

    %% Special
    encode_syscall/0,
    encode_nop/0,
    encode_int3/0,

    %% Memory
    encode_lea_rip_rel/2,
    encode_mov_mem_load/3,
    encode_mov_mem_store/3,

    %% Privileged: Control Registers
    encode_mov_from_cr/2,
    encode_mov_to_cr/2,

    %% Privileged: Descriptor Tables
    encode_lgdt/1,
    encode_lidt/1,
    encode_sgdt/1,
    encode_sidt/1,
    encode_ltr/1,

    %% Privileged: Interrupts
    encode_cli/0,
    encode_sti/0,
    encode_iretq/0,
    encode_hlt/0,
    encode_int/1,

    %% Privileged: I/O Ports
    encode_inb_dx/0,
    encode_inw_dx/0,
    encode_ind_dx/0,
    encode_outb_dx/0,
    encode_outw_dx/0,
    encode_outd_dx/0,
    encode_inb_imm/1,
    encode_outb_imm/1,

    %% Privileged: MSR
    encode_rdmsr/0,
    encode_wrmsr/0,

    %% Privileged: TLB
    encode_invlpg/1,

    %% Privileged: Atomic
    encode_lock_prefix/0,
    encode_cmpxchg_rr/2,
    encode_xchg_rr/2,

    %% Privileged: Segment
    encode_mov_from_seg/2,
    encode_swapgs/0,

    %% Privileged: Misc
    encode_cpuid/0,
    encode_rdtsc/0,
    encode_mfence/0,
    encode_lfence/0,
    encode_sfence/0,
    encode_pause/0,
    encode_ud2/0,
    encode_wbinvd/0,

    %% Condition codes
    cond_code/1,

    %% ABI register sets
    system_v_arg_regs/0,
    callee_saved_regs/0,
    caller_saved_regs/0
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type reg64() :: rax | rcx | rdx | rbx | rsp | rbp | rsi | rdi
               | r8  | r9  | r10 | r11 | r12 | r13 | r14 | r15.

-type reg32() :: eax | ecx | edx | ebx | esp | ebp | esi | edi
               | r8d | r9d | r10d | r11d | r12d | r13d | r14d | r15d.

-type reg() :: reg64() | reg32().

-type condition() :: eq | ne | lt | le | gt | ge | ltu | leu | gtu | geu.

-type control_reg() :: cr0 | cr2 | cr3 | cr4 | cr8.

-type segment_reg() :: es | cs | ss | ds | fs | gs.

-export_type([reg64/0, reg32/0, reg/0, condition/0, control_reg/0, segment_reg/0]).

%%====================================================================
%% Register encoding
%%====================================================================

%% @doc Map register atom to 0-15 integer code.
-spec reg_code(reg()) -> 0..15.
reg_code(rax) -> 0;
reg_code(rcx) -> 1;
reg_code(rdx) -> 2;
reg_code(rbx) -> 3;
reg_code(rsp) -> 4;
reg_code(rbp) -> 5;
reg_code(rsi) -> 6;
reg_code(rdi) -> 7;
reg_code(r8)  -> 8;
reg_code(r9)  -> 9;
reg_code(r10) -> 10;
reg_code(r11) -> 11;
reg_code(r12) -> 12;
reg_code(r13) -> 13;
reg_code(r14) -> 14;
reg_code(r15) -> 15;
%% 32-bit register names map to the same codes.
reg_code(eax) -> 0;
reg_code(ecx) -> 1;
reg_code(edx) -> 2;
reg_code(ebx) -> 3;
reg_code(esp) -> 4;
reg_code(ebp) -> 5;
reg_code(esi) -> 6;
reg_code(edi) -> 7;
reg_code(r8d)  -> 8;
reg_code(r9d)  -> 9;
reg_code(r10d) -> 10;
reg_code(r11d) -> 11;
reg_code(r12d) -> 12;
reg_code(r13d) -> 13;
reg_code(r14d) -> 14;
reg_code(r15d) -> 15.

%% @doc Lower 3 bits of register code (for ModR/M, SIB fields).
-spec reg_lo(reg()) -> 0..7.
reg_lo(Reg) ->
    reg_code(Reg) band 7.

%% @doc Extension bit of register code (for REX.R, REX.B, REX.X).
-spec reg_hi(reg()) -> 0..1.
reg_hi(Reg) ->
    (reg_code(Reg) bsr 3) band 1.

%% @doc Whether this register needs a REX prefix for its extension bit.
-spec needs_rex(reg()) -> boolean().
needs_rex(Reg) ->
    reg_code(Reg) >= 8.

%% @doc Map control register atom to 0-15 integer code.
-spec cr_code(control_reg()) -> 0..15.
cr_code(cr0) -> 0;
cr_code(cr2) -> 2;
cr_code(cr3) -> 3;
cr_code(cr4) -> 4;
cr_code(cr8) -> 8.

%% @doc Map segment register atom to 0-5 integer code.
-spec seg_code(segment_reg()) -> 0..5.
seg_code(es) -> 0;
seg_code(cs) -> 1;
seg_code(ss) -> 2;
seg_code(ds) -> 3;
seg_code(fs) -> 4;
seg_code(gs) -> 5.

%%====================================================================
%% Encoding helpers
%%====================================================================

%% @doc Build a REX prefix byte. W, R, X, B are 0 or 1.
-spec rex(0..1, 0..1, 0..1, 0..1) -> byte().
rex(W, R, X, B) ->
    16#40 bor (W bsl 3) bor (R bsl 2) bor (X bsl 1) bor B.

%% @doc REX.W prefix with optional B extension from Reg.
%%      Used for single-operand instructions needing 64-bit operand size.
-spec rex_w(reg()) -> byte().
rex_w(Reg) ->
    rex(1, 0, 0, reg_hi(Reg)).

%% @doc Build a ModR/M byte. Mod is 2 bits, Reg and RM are 3 bits each.
-spec modrm(0..3, 0..7, 0..7) -> byte().
modrm(Mod, Reg, RM) ->
    (Mod bsl 6) bor (Reg bsl 3) bor RM.

%%====================================================================
%% Condition codes for Jcc
%%====================================================================

%% @doc Map condition atom to the second byte of the 0F XX Jcc opcode.
%%      The full two-byte opcode is 0F <cond_code(Cond)>.
-spec cond_code(condition()) -> byte().
cond_code(eq)  -> 16#84;   %% JE / JZ
cond_code(ne)  -> 16#85;   %% JNE / JNZ
cond_code(lt)  -> 16#8C;   %% JL (signed)
cond_code(ge)  -> 16#8D;   %% JGE (signed)
cond_code(le)  -> 16#8E;   %% JLE (signed)
cond_code(gt)  -> 16#8F;   %% JG (signed)
cond_code(ltu) -> 16#82;   %% JB / JNAE (unsigned below)
cond_code(geu) -> 16#83;   %% JAE / JNB (unsigned above-or-equal)
cond_code(nc)  -> 16#83;   %% JNC / JNAE (no carry, same as geu)
cond_code(leu) -> 16#86;   %% JBE / JNA (unsigned below-or-equal)
cond_code(gtu) -> 16#87.   %% JA / JNBE (unsigned above)

%%====================================================================
%% ABI register sets (System V AMD64 ABI)
%%====================================================================

%% @doc Integer argument registers in calling order.
-spec system_v_arg_regs() -> [reg64()].
system_v_arg_regs() ->
    [rdi, rsi, rdx, rcx, r8, r9].

%% @doc Callee-saved registers (must be preserved across calls).
-spec callee_saved_regs() -> [reg64()].
callee_saved_regs() ->
    [rbx, r12, r13, r14, r15].

%% @doc Caller-saved registers (may be clobbered by calls).
-spec caller_saved_regs() -> [reg64()].
caller_saved_regs() ->
    [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11].

%%====================================================================
%% MOV instructions
%%====================================================================

%% @doc MOV r64, r64
%%      Encoding: REX.W + 89 /r
%%      The /r means Src goes in ModR/M reg field, Dst in r/m field.
-spec encode_mov_rr(reg64(), reg64()) -> binary().
encode_mov_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#89:8, ModRM:8>>.

%% @doc MOV r64, imm64  (movabs)
%%      Encoding: REX.W + B8+rd io  (register in opcode, 8-byte immediate)
-spec encode_mov_imm64(reg64(), integer()) -> binary().
encode_mov_imm64(Dst, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    Opcode = 16#B8 + reg_lo(Dst),
    <<Rex:8, Opcode:8, Imm:64/little-signed>>.

%% @doc MOV r64, imm32  (sign-extended)
%%      Encoding: REX.W + C7 /0 id
-spec encode_mov_imm32(reg64(), integer()) -> binary().
encode_mov_imm32(Dst, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 0, reg_lo(Dst)),
    <<Rex:8, 16#C7:8, ModRM:8, Imm:32/little-signed>>.

%%====================================================================
%% ADD instructions
%%====================================================================

%% @doc ADD r64, r64
%%      Encoding: REX.W + 01 /r
-spec encode_add_rr(reg64(), reg64()) -> binary().
encode_add_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#01:8, ModRM:8>>.

%% @doc ADD r64, imm32
%%      Encoding: REX.W + 81 /0 id
-spec encode_add_imm(reg64(), integer()) -> binary().
encode_add_imm(Dst, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 0, reg_lo(Dst)),
    <<Rex:8, 16#81:8, ModRM:8, Imm:32/little-signed>>.

%%====================================================================
%% SUB instructions
%%====================================================================

%% @doc SUB r64, r64
%%      Encoding: REX.W + 29 /r
-spec encode_sub_rr(reg64(), reg64()) -> binary().
encode_sub_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#29:8, ModRM:8>>.

%% @doc SUB r64, imm32
%%      Encoding: REX.W + 81 /5 id
-spec encode_sub_imm(reg64(), integer()) -> binary().
encode_sub_imm(Dst, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 5, reg_lo(Dst)),
    <<Rex:8, 16#81:8, ModRM:8, Imm:32/little-signed>>.

%%====================================================================
%% MUL / DIV instructions
%%====================================================================

%% @doc IMUL r64, r64  (two-operand signed multiply)
%%      Encoding: REX.W + 0F AF /r
%%      Result goes into Dst.
-spec encode_imul_rr(reg64(), reg64()) -> binary().
encode_imul_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Dst), 0, reg_hi(Src)),
    ModRM = modrm(2#11, reg_lo(Dst), reg_lo(Src)),
    <<Rex:8, 16#0F:8, 16#AF:8, ModRM:8>>.

%% @doc IDIV r64  (signed divide RDX:RAX by r64)
%%      Encoding: REX.W + F7 /7
%%      Quotient in RAX, remainder in RDX.
-spec encode_idiv(reg64()) -> binary().
encode_idiv(Src) ->
    Rex = rex(1, 0, 0, reg_hi(Src)),
    ModRM = modrm(2#11, 7, reg_lo(Src)),
    <<Rex:8, 16#F7:8, ModRM:8>>.

%% @doc CQO — sign-extend RAX into RDX:RAX
%%      Encoding: 48 99
-spec encode_cqo() -> binary().
encode_cqo() ->
    <<16#48:8, 16#99:8>>.

%%====================================================================
%% Bitwise instructions
%%====================================================================

%% @doc AND r64, r64
%%      Encoding: REX.W + 21 /r
-spec encode_and_rr(reg64(), reg64()) -> binary().
encode_and_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#21:8, ModRM:8>>.

%% @doc OR r64, r64
%%      Encoding: REX.W + 09 /r
-spec encode_or_rr(reg64(), reg64()) -> binary().
encode_or_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#09:8, ModRM:8>>.

%% @doc XOR r64, r64
%%      Encoding: REX.W + 31 /r
-spec encode_xor_rr(reg64(), reg64()) -> binary().
encode_xor_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#31:8, ModRM:8>>.

%% @doc SHL r64, CL  (shift left by CL register)
%%      Encoding: REX.W + D3 /4
-spec encode_shl_cl(reg64()) -> binary().
encode_shl_cl(Dst) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 4, reg_lo(Dst)),
    <<Rex:8, 16#D3:8, ModRM:8>>.

%% @doc SHR r64, CL  (logical shift right by CL)
%%      Encoding: REX.W + D3 /5
-spec encode_shr_cl(reg64()) -> binary().
encode_shr_cl(Dst) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 5, reg_lo(Dst)),
    <<Rex:8, 16#D3:8, ModRM:8>>.

%% @doc SAR r64, CL  (arithmetic shift right by CL)
%%      Encoding: REX.W + D3 /7
-spec encode_sar_cl(reg64()) -> binary().
encode_sar_cl(Dst) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 7, reg_lo(Dst)),
    <<Rex:8, 16#D3:8, ModRM:8>>.

%% @doc NEG r64  (two's complement negate)
%%      Encoding: REX.W + F7 /3
-spec encode_neg(reg64()) -> binary().
encode_neg(Dst) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 3, reg_lo(Dst)),
    <<Rex:8, 16#F7:8, ModRM:8>>.

%% @doc NOT r64  (bitwise complement)
%%      Encoding: REX.W + F7 /2
-spec encode_not(reg64()) -> binary().
encode_not(Dst) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 2, reg_lo(Dst)),
    <<Rex:8, 16#F7:8, ModRM:8>>.

%%====================================================================
%% Compare / Test instructions
%%====================================================================

%% @doc CMP r64, r64
%%      Encoding: REX.W + 39 /r
-spec encode_cmp_rr(reg64(), reg64()) -> binary().
encode_cmp_rr(A, B) ->
    Rex = rex(1, reg_hi(B), 0, reg_hi(A)),
    ModRM = modrm(2#11, reg_lo(B), reg_lo(A)),
    <<Rex:8, 16#39:8, ModRM:8>>.

%% @doc CMP r64, imm32
%%      Encoding: REX.W + 81 /7 id
-spec encode_cmp_imm(reg64(), integer()) -> binary().
encode_cmp_imm(Dst, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, 7, reg_lo(Dst)),
    <<Rex:8, 16#81:8, ModRM:8, Imm:32/little-signed>>.

%% @doc TEST r64, r64
%%      Encoding: REX.W + 85 /r
-spec encode_test_rr(reg64(), reg64()) -> binary().
encode_test_rr(A, B) ->
    Rex = rex(1, reg_hi(B), 0, reg_hi(A)),
    ModRM = modrm(2#11, reg_lo(B), reg_lo(A)),
    <<Rex:8, 16#85:8, ModRM:8>>.

%%====================================================================
%% Control flow instructions
%%====================================================================

%% @doc JMP rel32
%%      Encoding: E9 cd
%%      Offset is relative to end of this instruction (5 bytes).
-spec encode_jmp_rel32(integer()) -> binary().
encode_jmp_rel32(Offset) ->
    <<16#E9:8, Offset:32/little-signed>>.

%% @doc Jcc rel32 (conditional jump)
%%      Encoding: 0F 8x cd
%%      Offset is relative to end of this instruction (6 bytes).
-spec encode_jcc_rel32(condition(), integer()) -> binary().
encode_jcc_rel32(Cond, Offset) ->
    CC = cond_code(Cond),
    <<16#0F:8, CC:8, Offset:32/little-signed>>.

%% @doc JNS rel32 (Jump if Not Sign)
%%      Encoding: 0F 89 cd
%%      Offset is relative to end of this instruction (6 bytes).
-spec encode_jns(integer()) -> binary().
encode_jns(Offset) ->
    <<16#0F:8, 16#89:8, Offset:32/little-signed>>.

%% @doc JLE rel32 (Jump if Less or Equal)
%%      Encoding: 0F 8E cd
%%      Offset is relative to end of this instruction (6 bytes).
-spec encode_jle(integer()) -> binary().
encode_jle(Offset) ->
    <<16#0F:8, 16#8E:8, Offset:32/little-signed>>.

%% @doc CALL rel32
%%      Encoding: E8 cd
%%      Offset is relative to end of this instruction (5 bytes).
-spec encode_call_rel32(integer()) -> binary().
encode_call_rel32(Offset) ->
    <<16#E8:8, Offset:32/little-signed>>.

%% @doc CALL r64  (indirect call through register)
%%      Encoding: [REX] FF /2
-spec encode_call_reg(reg64()) -> binary().
encode_call_reg(Reg) ->
    ModRM = modrm(2#11, 2, reg_lo(Reg)),
    case needs_rex(Reg) of
        true ->
            Rex = rex(0, 0, 0, reg_hi(Reg)),
            <<Rex:8, 16#FF:8, ModRM:8>>;
        false ->
            <<16#FF:8, ModRM:8>>
    end.

%% @doc RET
%%      Encoding: C3
-spec encode_ret() -> binary().
encode_ret() ->
    <<16#C3:8>>.

%%====================================================================
%% Stack instructions
%%====================================================================

%% @doc PUSH r64
%%      Encoding: [REX] 50+rd
%%      REX prefix needed only for r8-r15.
-spec encode_push(reg64()) -> binary().
encode_push(Reg) ->
    Opcode = 16#50 + reg_lo(Reg),
    case needs_rex(Reg) of
        true ->
            Rex = rex(0, 0, 0, 1),
            <<Rex:8, Opcode:8>>;
        false ->
            <<Opcode:8>>
    end.

%% @doc POP r64
%%      Encoding: [REX] 58+rd
%%      REX prefix needed only for r8-r15.
-spec encode_pop(reg64()) -> binary().
encode_pop(Reg) ->
    Opcode = 16#58 + reg_lo(Reg),
    case needs_rex(Reg) of
        true ->
            Rex = rex(0, 0, 0, 1),
            <<Rex:8, Opcode:8>>;
        false ->
            <<Opcode:8>>
    end.

%% @doc PUSH imm32
%%      Encoding: 68 id (push imm32, sign-extended to 64-bit on stack)
-spec encode_push_imm32(integer()) -> binary().
encode_push_imm32(Imm) when Imm >= -16#80000000, Imm =< 16#7FFFFFFF ->
    <<16#68:8, Imm:32/signed-little>>.

%%====================================================================
%% Special instructions
%%====================================================================

%% @doc SYSCALL
%%      Encoding: 0F 05
-spec encode_syscall() -> binary().
encode_syscall() ->
    <<16#0F:8, 16#05:8>>.

%% @doc NOP
%%      Encoding: 90
-spec encode_nop() -> binary().
encode_nop() ->
    <<16#90:8>>.

%% @doc INT3 (Breakpoint)
%%      Encoding: CC
-spec encode_int3() -> binary().
encode_int3() ->
    <<16#CC:8>>.

%%====================================================================
%% Memory instructions
%%====================================================================

%% @doc LEA r64, [RIP + disp32]
%%      Encoding: REX.W + 8D /r  with ModR/M [Mod=00, R/M=101 (RIP-relative)]
%%      Used for position-independent code data references.
-spec encode_lea_rip_rel(reg64(), integer()) -> binary().
encode_lea_rip_rel(Dst, Offset) ->
    Rex = rex(1, reg_hi(Dst), 0, 0),
    %% Mod=00, Reg=Dst, R/M=101 (RIP-relative)
    ModRM = modrm(2#00, reg_lo(Dst), 2#101),
    <<Rex:8, 16#8D:8, ModRM:8, Offset:32/little-signed>>.

%% @doc MOV r64, [Base + disp]  (load from memory)
%%      Encoding: REX.W + 8B /r  with appropriate ModR/M and optional SIB
%%
%%      ModR/M addressing modes:
%%        Mod=00: [r/m]          (no disp, except rbp=disp32, rsp=SIB)
%%        Mod=01: [r/m + disp8]
%%        Mod=10: [r/m + disp32]
%%      If Base is rsp, a SIB byte is required: SIB=0x24 (scale=1, idx=none, base=rsp)
-spec encode_mov_mem_load(reg64(), reg64(), integer()) -> binary().
encode_mov_mem_load(Dst, Base, Offset) ->
    encode_mem_op(16#8B, Dst, Base, Offset).

%% @doc MOV [Base + disp], r64  (store to memory)
%%      Encoding: REX.W + 89 /r  with appropriate ModR/M and optional SIB
-spec encode_mov_mem_store(reg64(), integer(), reg64()) -> binary().
encode_mov_mem_store(Base, Offset, Src) ->
    encode_mem_op(16#89, Src, Base, Offset).

%%====================================================================
%% Privileged: Control Register Access
%%====================================================================

%% @doc MOV r64, CRn  (read control register)
%%      Encoding: 0F 20 /r
%%      CRn in ModR/M reg field, GPR in r/m field.
%%      For CR8+, use REX.R bit for high bit of CR register.
-spec encode_mov_from_cr(reg64(), control_reg()) -> binary().
encode_mov_from_cr(Dst, CRn) ->
    CrCode = cr_code(CRn),
    CrHi = (CrCode bsr 3) band 1,
    CrLo = CrCode band 7,
    Rex = rex(1, CrHi, 0, reg_hi(Dst)),
    ModRM = modrm(2#11, CrLo, reg_lo(Dst)),
    <<Rex:8, 16#0F:8, 16#20:8, ModRM:8>>.

%% @doc MOV CRn, r64  (write control register)
%%      Encoding: 0F 22 /r
%%      CRn in ModR/M reg field, GPR in r/m field.
%%      For CR8+, use REX.R bit for high bit of CR register.
-spec encode_mov_to_cr(control_reg(), reg64()) -> binary().
encode_mov_to_cr(CRn, Src) ->
    CrCode = cr_code(CRn),
    CrHi = (CrCode bsr 3) band 1,
    CrLo = CrCode band 7,
    Rex = rex(1, CrHi, 0, reg_hi(Src)),
    ModRM = modrm(2#11, CrLo, reg_lo(Src)),
    <<Rex:8, 16#0F:8, 16#22:8, ModRM:8>>.

%%====================================================================
%% Privileged: Descriptor Table Operations
%%====================================================================

%% @doc LGDT m  (load GDT register from memory)
%%      Encoding: 0F 01 /2
%%      MemAddr is [Base + Offset] addressing.
-spec encode_lgdt({reg64(), integer()}) -> binary().
encode_lgdt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 2, Base, Offset).

%% @doc LIDT m  (load IDT register from memory)
%%      Encoding: 0F 01 /3
-spec encode_lidt({reg64(), integer()}) -> binary().
encode_lidt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 3, Base, Offset).

%% @doc SGDT m  (store GDT register to memory)
%%      Encoding: 0F 01 /0
-spec encode_sgdt({reg64(), integer()}) -> binary().
encode_sgdt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 0, Base, Offset).

%% @doc SIDT m  (store IDT register to memory)
%%      Encoding: 0F 01 /1
-spec encode_sidt({reg64(), integer()}) -> binary().
encode_sidt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 1, Base, Offset).

%% @doc LTR r/m16  (load task register)
%%      Encoding: 0F 00 /3
%%      Using 64-bit register for simplicity (low 16 bits used).
-spec encode_ltr(reg64()) -> binary().
encode_ltr(Src) ->
    Rex = rex(0, 0, 0, reg_hi(Src)),
    ModRM = modrm(2#11, 3, reg_lo(Src)),
    <<Rex:8, 16#0F:8, 16#00:8, ModRM:8>>.

%%====================================================================
%% Privileged: Interrupt Control
%%====================================================================

%% @doc CLI  (clear interrupt flag)
%%      Encoding: FA
-spec encode_cli() -> binary().
encode_cli() ->
    <<16#FA:8>>.

%% @doc STI  (set interrupt flag)
%%      Encoding: FB
-spec encode_sti() -> binary().
encode_sti() ->
    <<16#FB:8>>.

%% @doc IRETQ  (interrupt return 64-bit)
%%      Encoding: 48 CF
-spec encode_iretq() -> binary().
encode_iretq() ->
    <<16#48:8, 16#CF:8>>.

%% @doc HLT  (halt processor)
%%      Encoding: F4
-spec encode_hlt() -> binary().
encode_hlt() ->
    <<16#F4:8>>.

%% @doc INT imm8  (software interrupt)
%%      Encoding: CD ib
-spec encode_int(0..255) -> binary().
encode_int(N) ->
    <<16#CD:8, N:8>>.

%%====================================================================
%% Privileged: I/O Port Access
%%====================================================================

%% @doc IN AL, DX  (input byte from port in DX)
%%      Encoding: EC
-spec encode_inb_dx() -> binary().
encode_inb_dx() ->
    <<16#EC:8>>.

%% @doc IN AX, DX  (input word from port in DX)
%%      Encoding: 66 ED
-spec encode_inw_dx() -> binary().
encode_inw_dx() ->
    <<16#66:8, 16#ED:8>>.

%% @doc IN EAX, DX  (input dword from port in DX)
%%      Encoding: ED
-spec encode_ind_dx() -> binary().
encode_ind_dx() ->
    <<16#ED:8>>.

%% @doc OUT DX, AL  (output byte to port in DX)
%%      Encoding: EE
-spec encode_outb_dx() -> binary().
encode_outb_dx() ->
    <<16#EE:8>>.

%% @doc OUT DX, AX  (output word to port in DX)
%%      Encoding: 66 EF
-spec encode_outw_dx() -> binary().
encode_outw_dx() ->
    <<16#66:8, 16#EF:8>>.

%% @doc OUT DX, EAX  (output dword to port in DX)
%%      Encoding: EF
-spec encode_outd_dx() -> binary().
encode_outd_dx() ->
    <<16#EF:8>>.

%% @doc IN AL, imm8  (input byte from immediate port)
%%      Encoding: E4 ib
-spec encode_inb_imm(0..255) -> binary().
encode_inb_imm(Port) ->
    <<16#E4:8, Port:8>>.

%% @doc OUT imm8, AL  (output byte to immediate port)
%%      Encoding: E6 ib
-spec encode_outb_imm(0..255) -> binary().
encode_outb_imm(Port) ->
    <<16#E6:8, Port:8>>.

%%====================================================================
%% Privileged: MSR Access
%%====================================================================

%% @doc RDMSR  (read MSR indexed by ECX into EDX:EAX)
%%      Encoding: 0F 32
-spec encode_rdmsr() -> binary().
encode_rdmsr() ->
    <<16#0F:8, 16#32:8>>.

%% @doc WRMSR  (write EDX:EAX to MSR indexed by ECX)
%%      Encoding: 0F 30
-spec encode_wrmsr() -> binary().
encode_wrmsr() ->
    <<16#0F:8, 16#30:8>>.

%%====================================================================
%% Privileged: TLB Management
%%====================================================================

%% @doc INVLPG m  (invalidate TLB entry)
%%      Encoding: 0F 01 /7
%%      MemAddr is [Base + Offset] addressing.
-spec encode_invlpg({reg64(), integer()}) -> binary().
encode_invlpg({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 7, Base, Offset).

%%====================================================================
%% Privileged: Atomic Operations
%%====================================================================

%% @doc LOCK prefix
%%      Encoding: F0
%%      Prepend to instruction for atomic memory operation.
-spec encode_lock_prefix() -> binary().
encode_lock_prefix() ->
    <<16#F0:8>>.

%% @doc CMPXCHG r/m64, r64  (compare and exchange)
%%      Encoding: REX.W + 0F B1 /r
%%      Compare RAX with [Dst]; if equal, load Src into [Dst]; else load [Dst] into RAX.
-spec encode_cmpxchg_rr(reg64(), reg64()) -> binary().
encode_cmpxchg_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#0F:8, 16#B1:8, ModRM:8>>.

%% @doc XCHG r64, r64  (atomic exchange)
%%      Encoding: REX.W + 87 /r
-spec encode_xchg_rr(reg64(), reg64()) -> binary().
encode_xchg_rr(Dst, Src) ->
    Rex = rex(1, reg_hi(Src), 0, reg_hi(Dst)),
    ModRM = modrm(2#11, reg_lo(Src), reg_lo(Dst)),
    <<Rex:8, 16#87:8, ModRM:8>>.

%%====================================================================
%% Privileged: Segment Register
%%====================================================================

%% @doc MOV r64, SegReg  (read segment register)
%%      Encoding: 8C /r
%%      SegReg in ModR/M reg field, GPR in r/m field.
%%      CRITICAL FIX (Round 28, Finding 6): Always emit REX.W for 64-bit form
-spec encode_mov_from_seg(reg64(), segment_reg()) -> binary().
encode_mov_from_seg(Dst, SegReg) ->
    ModRM = modrm(2#11, seg_code(SegReg), reg_lo(Dst)),
    %% Always emit REX.W (0x48 + B bit) for 64-bit operand size
    Rex = rex(1, 0, 0, reg_hi(Dst)),
    <<Rex:8, 16#8C:8, ModRM:8>>.

%% @doc SWAPGS  (swap GS base register with KernelGSBase MSR)
%%      Encoding: 0F 01 F8
-spec encode_swapgs() -> binary().
encode_swapgs() ->
    <<16#0F:8, 16#01:8, 16#F8:8>>.

%%====================================================================
%% Privileged: Miscellaneous
%%====================================================================

%% @doc CPUID  (CPU identification)
%%      Encoding: 0F A2
-spec encode_cpuid() -> binary().
encode_cpuid() ->
    <<16#0F:8, 16#A2:8>>.

%% @doc RDTSC  (read timestamp counter into EDX:EAX)
%%      Encoding: 0F 31
-spec encode_rdtsc() -> binary().
encode_rdtsc() ->
    <<16#0F:8, 16#31:8>>.

%% @doc MFENCE  (memory fence - full serialization)
%%      Encoding: 0F AE F0
-spec encode_mfence() -> binary().
encode_mfence() ->
    <<16#0F:8, 16#AE:8, 16#F0:8>>.

%% @doc LFENCE  (load fence - serializes loads)
%%      Encoding: 0F AE E8
-spec encode_lfence() -> binary().
encode_lfence() ->
    <<16#0F:8, 16#AE:8, 16#E8:8>>.

%% @doc SFENCE  (store fence - serializes stores)
%%      Encoding: 0F AE F8
-spec encode_sfence() -> binary().
encode_sfence() ->
    <<16#0F:8, 16#AE:8, 16#F8:8>>.

%% @doc PAUSE  (spin-loop hint)
%%      Encoding: F3 90
-spec encode_pause() -> binary().
encode_pause() ->
    <<16#F3:8, 16#90:8>>.

%% @doc UD2  (undefined instruction / debug trap)
%%      Encoding: 0F 0B
-spec encode_ud2() -> binary().
encode_ud2() ->
    <<16#0F:8, 16#0B:8>>.

%% @doc WBINVD  (write back and invalidate cache)
%%      Encoding: 0F 09
-spec encode_wbinvd() -> binary().
encode_wbinvd() ->
    <<16#0F:8, 16#09:8>>.

%%====================================================================
%% Internal: memory addressing encoder
%%====================================================================

%% @doc Encode a memory operation with [Base + Offset] addressing.
%%      Opcode byte provided by caller; RegField is the /r register.
%%      Handles SIB byte for rsp-based addressing and displacement sizing.
-spec encode_mem_op(byte(), reg64(), reg64(), integer()) -> binary().
encode_mem_op(Opcode, RegField, Base, 0) when Base =/= rbp, Base =/= r13 ->
    %% Mod=00, no displacement (except rbp/r13 which need disp8=0)
    Rex = rex(1, reg_hi(RegField), 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            %% rsp/r12 as base requires SIB byte
            ModRM = modrm(2#00, reg_lo(RegField), 2#100),
            SIB = 16#24,  %% scale=00, index=100(none), base=100(rsp)
            <<Rex:8, Opcode:8, ModRM:8, SIB:8>>;
        false ->
            ModRM = modrm(2#00, reg_lo(RegField), reg_lo(Base)),
            <<Rex:8, Opcode:8, ModRM:8>>
    end;
encode_mem_op(Opcode, RegField, Base, Offset) when Offset >= -128, Offset =< 127 ->
    %% Mod=01, disp8
    Rex = rex(1, reg_hi(RegField), 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#01, reg_lo(RegField), 2#100),
            SIB = 16#24,
            <<Rex:8, Opcode:8, ModRM:8, SIB:8, Offset:8/little-signed>>;
        false ->
            ModRM = modrm(2#01, reg_lo(RegField), reg_lo(Base)),
            <<Rex:8, Opcode:8, ModRM:8, Offset:8/little-signed>>
    end;
encode_mem_op(Opcode, RegField, Base, Offset) ->
    %% Mod=10, disp32
    Rex = rex(1, reg_hi(RegField), 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#10, reg_lo(RegField), 2#100),
            SIB = 16#24,
            <<Rex:8, Opcode:8, ModRM:8, SIB:8, Offset:32/little-signed>>;
        false ->
            ModRM = modrm(2#10, reg_lo(RegField), reg_lo(Base)),
            <<Rex:8, Opcode:8, ModRM:8, Offset:32/little-signed>>
    end.

%% @doc Check if register is rsp or r12 (both encode to r/m=100, requiring SIB).
-spec is_rsp_base(reg64()) -> boolean().
is_rsp_base(rsp) -> true;
is_rsp_base(r12) -> true;
is_rsp_base(_)   -> false.

%% @doc Encode a memory operation with extended opcode (0F + secondary opcode).
%%      Used for LGDT, LIDT, SGDT, SIDT, INVLPG which use 0F XX /digit encodings.
%%      ExtOpcode is the second byte (e.g., 01 for descriptor table ops).
%%      RegField is the /digit value from the instruction encoding.
-spec encode_mem_op_ext(byte(), byte(), 0..7, reg64(), integer()) -> binary().
encode_mem_op_ext(Prefix, ExtOpcode, RegField, Base, 0) when Base =/= rbp, Base =/= r13 ->
    %% Mod=00, no displacement
    Rex = rex(1, 0, 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#00, RegField, 2#100),
            SIB = 16#24,
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8, SIB:8>>;
        false ->
            ModRM = modrm(2#00, RegField, reg_lo(Base)),
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8>>
    end;
encode_mem_op_ext(Prefix, ExtOpcode, RegField, Base, Offset) when Offset >= -128, Offset =< 127 ->
    %% Mod=01, disp8
    Rex = rex(1, 0, 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#01, RegField, 2#100),
            SIB = 16#24,
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8, SIB:8, Offset:8/little-signed>>;
        false ->
            ModRM = modrm(2#01, RegField, reg_lo(Base)),
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8, Offset:8/little-signed>>
    end;
encode_mem_op_ext(Prefix, ExtOpcode, RegField, Base, Offset) ->
    %% Mod=10, disp32
    Rex = rex(1, 0, 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#10, RegField, 2#100),
            SIB = 16#24,
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8, SIB:8, Offset:32/little-signed>>;
        false ->
            ModRM = modrm(2#10, RegField, reg_lo(Base)),
            <<Rex:8, Prefix:8, ExtOpcode:8, ModRM:8, Offset:32/little-signed>>
    end.
