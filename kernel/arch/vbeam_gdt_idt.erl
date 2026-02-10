%%% @doc GDT and IDT setup module for bare-metal x86_64.
%%% Generates machine code to set up Global Descriptor Table and Interrupt Descriptor Table.
%%% @end
-module(vbeam_gdt_idt).

-export([
    gdt_data/0,
    gdt_load_code/1,
    idt_data/1,
    idt_load_code/1,
    exception_stubs/0
]).

%%====================================================================
%% GDT (Global Descriptor Table)
%%====================================================================

%% @doc Returns binary containing a flat GDT for long mode.
%%      5 entries + GDTR structure:
%%      - Entry 0: Null descriptor (required by x86_64)
%%      - Entry 1: Code segment ring 0 (kernel code)
%%      - Entry 2: Data segment ring 0 (kernel data)
%%      - Entry 3: Code segment ring 3 (user code)
%%      - Entry 4: Data segment ring 3 (user data)
%%      - GDTR: 6 bytes (2-byte limit + 4-byte base)
-spec gdt_data() -> binary().
gdt_data() ->
    %% Entry 0: Null descriptor (8 bytes of zeros)
    Null = <<0:64>>,

    %% Entry 1: Code segment ring 0
    %% Base=0, Limit=0xFFFFF (4GB with 4KB granularity)
    %% Access byte: Present=1, DPL=00 (ring 0), Type=1 (code/data), Executable=1,
    %%              Conforming=0, Readable=1, Accessed=0 = 0x9A
    %% Flags: Granularity=1 (4KB), Long mode=1, Reserved=0, AVL=0 = 0xA
    KernelCode = build_gdt_entry(0, 16#FFFFF, 16#9A, 16#A),

    %% Entry 2: Data segment ring 0
    %% Access byte: Present=1, DPL=00, Type=1, Executable=0,
    %%              Direction=0, Writable=1, Accessed=0 = 0x92
    %% Flags: Granularity=1 (4KB), Long mode=0 (L-bit for data), Reserved=0, AVL=0 = 0x8
    KernelData = build_gdt_entry(0, 16#FFFFF, 16#92, 16#8),

    %% Entry 3: Code segment ring 3 (user)
    %% Access byte: Present=1, DPL=11 (ring 3), Type=1, Executable=1,
    %%              Conforming=0, Readable=1, Accessed=0 = 0xFA
    UserCode = build_gdt_entry(0, 16#FFFFF, 16#FA, 16#A),

    %% Entry 4: Data segment ring 3 (user)
    %% Access byte: Present=1, DPL=11, Type=1, Executable=0,
    %%              Direction=0, Writable=1, Accessed=0 = 0xF2
    %% Flags: Granularity=1 (4KB), Long mode=0 (L-bit for data), Reserved=0, AVL=0 = 0x8
    UserData = build_gdt_entry(0, 16#FFFFF, 16#F2, 16#8),

    %% GDTR structure (10 bytes total)
    %% Limit: size - 1 = (5 entries * 8) - 1 = 39
    %% Base: 8-byte address (placeholder, will be set at load time)
    Limit = 39,
    Base = 0,  %% Placeholder
    GDTR = <<Limit:16/little, Base:64/little>>,

    iolist_to_binary([Null, KernelCode, KernelData, UserCode, UserData, GDTR]).

%% @doc Build a single 8-byte GDT entry.
%%      Base: 32-bit linear base address
%%      Limit: 20-bit limit value
%%      Access: 8-bit access byte
%%      Flags: 4-bit flags (upper nibble)
-spec build_gdt_entry(non_neg_integer(), non_neg_integer(), byte(), 0..15) -> binary().
build_gdt_entry(Base, Limit, Access, Flags) ->
    %% GDT entry structure (8 bytes):
    %% Bytes 0-1: Limit bits 0-15
    %% Bytes 2-3: Base bits 0-15
    %% Byte 4:    Base bits 16-23
    %% Byte 5:    Access byte
    %% Byte 6:    Flags (upper 4 bits) | Limit bits 16-19 (lower 4 bits)
    %% Byte 7:    Base bits 24-31

    LimitLow = Limit band 16#FFFF,
    LimitHigh = (Limit bsr 16) band 16#F,

    BaseLow = Base band 16#FFFF,
    BaseMid = (Base bsr 16) band 16#FF,
    BaseHigh = (Base bsr 24) band 16#FF,

    FlagsAndLimit = (Flags bsl 4) bor LimitHigh,

    <<LimitLow:16/little, BaseLow:16/little, BaseMid:8, Access:8, FlagsAndLimit:8, BaseHigh:8>>.

%% @doc Returns x86_64 machine code to load the GDT and reload segment registers.
%%      Input: GDTBaseAddr = physical address of GDT data
%%
%%      Steps:
%%      1. Store GDT base address in GDTR structure
%%      2. Load GDTR via lgdt
%%      3. Reload CS via far return
%%      4. Reload DS, ES, FS, GS, SS with data selector
%%
%%      Selector values:
%%        CS = 0x08 (entry 1, ring 0)
%%        DS = 0x10 (entry 2, ring 0)
-spec gdt_load_code(non_neg_integer()) -> binary().
gdt_load_code(GDTBaseAddr) ->
    %% GDTR is at GDTBaseAddr + 40 (after 5 GDT entries)
    GDTRAddr = GDTBaseAddr + 40,

    %% Offset 2 within GDTR is where the 8-byte base address goes
    GDTRBaseFieldAddr = GDTRAddr + 2,

    iolist_to_binary([
        %% mov rax, GDTBaseAddr
        encode_mov_imm64(rax, GDTBaseAddr),

        %% mov rbx, GDTRBaseFieldAddr
        encode_mov_imm64(rbx, GDTRBaseFieldAddr),

        %% mov [rbx], rax  (store GDT base in GDTR structure)
        encode_mov_mem_store(rbx, 0, rax),

        %% mov rax, GDTRAddr  (address of GDTR for lgdt)
        encode_mov_imm64(rax, GDTRAddr),

        %% lgdt [rax]
        encode_lgdt({rax, 0}),

        %% Reload CS via far return
        %% push 0x08 (code selector)
        encode_push_imm32(16#08),

        %% lea rax, [rip + reload_segments_label]
        %% RIP-relative displacement = target - (lea_end_address)
        %% lea is 7 bytes, push rax is 1 byte, retfq is 2 bytes
        %% Target is right after retfq, so displacement = 1 + 2 = 3
        encode_lea_rip_rel(rax, 3),

        %% push rax  (return address)
        encode_push(rax),

        %% retfq  (far return: pops RIP and CS)
        encode_retfq(),

        %% reload_segments_label:
        %% Load data selector (0x10) into DS, ES, FS, GS, SS
        %% mov ax, 0x10
        encode_mov_ax_imm16(16#10),

        %% mov ds, ax
        encode_mov_ds_ax(),

        %% mov es, ax
        encode_mov_es_ax(),

        %% mov fs, ax
        encode_mov_fs_ax(),

        %% mov gs, ax
        encode_mov_gs_ax(),

        %% mov ss, ax
        encode_mov_ss_ax(),

        %% ret
        encode_ret()
    ]).

%%====================================================================
%% IDT (Interrupt Descriptor Table)
%%====================================================================

%% @doc Returns binary containing 256 IDT entries (16 bytes each = 4096 bytes total).
%%      Each entry is an interrupt gate pointing to an ISR stub.
%%      Input: ISRStubBaseAddr = physical address of exception_stubs code
-spec idt_data(non_neg_integer()) -> binary().
idt_data(ISRStubBaseAddr) ->
    %% Each exception stub is padded to 10 bytes
    StubSize = 10,

    %% Build entries 0-31: exception handlers
    Exceptions = [build_idt_entry(ISRStubBaseAddr + (N * StubSize), 16#08, 16#8E) || N <- lists:seq(0, 31)],

    %% Entry 32: Timer interrupt (variable size, but follows stubs)
    %% Timer stub starts after all 32 exception stubs
    TimerStubOffset = 32 * StubSize,
    Timer = build_idt_entry(ISRStubBaseAddr + TimerStubOffset, 16#08, 16#8E),

    %% Entry 33: Generic stub (find offset by measuring timer stub)
    %% Timer stub is complex, measure it
    TimerStubData = build_timer_stub(),
    GenericStubOffset = TimerStubOffset + byte_size(TimerStubData),
    Generic = [build_idt_entry(ISRStubBaseAddr + GenericStubOffset, 16#08, 16#8E) || _ <- lists:seq(33, 255)],

    iolist_to_binary([Exceptions, Timer, Generic]).

%% @doc Build a single 16-byte IDT entry (interrupt gate).
%%      Offset: 64-bit ISR address
%%      Selector: code segment selector (0x08 for kernel CS)
%%      TypeAttr: type and attributes byte (0x8E = interrupt gate, ring 0, present)
-spec build_idt_entry(non_neg_integer(), non_neg_integer(), byte()) -> binary().
build_idt_entry(Offset, Selector, TypeAttr) ->
    %% IDT entry structure (16 bytes):
    %% Bytes 0-1:   Offset bits 0-15
    %% Bytes 2-3:   Selector
    %% Byte 4:      IST (Interrupt Stack Table, 0 = don't switch stack)
    %% Byte 5:      Type and attributes
    %% Bytes 6-7:   Offset bits 16-31
    %% Bytes 8-11:  Offset bits 32-63
    %% Bytes 12-15: Reserved (must be 0)

    OffsetLow = Offset band 16#FFFF,
    OffsetMid = (Offset bsr 16) band 16#FFFF,
    OffsetHigh = (Offset bsr 32) band 16#FFFFFFFF,

    IST = 0,  %% Don't switch stacks
    Reserved = 0,

    <<OffsetLow:16/little, Selector:16/little, IST:8, TypeAttr:8,
      OffsetMid:16/little, OffsetHigh:32/little, Reserved:32/little>>.

%% @doc Returns x86_64 machine code to load the IDT.
%%      Input: IDTBaseAddr = physical address of IDT data (4096 bytes)
-spec idt_load_code(non_neg_integer()) -> binary().
idt_load_code(IDTBaseAddr) ->
    %% Build IDTR structure in memory (10 bytes):
    %% Limit: size - 1 = (256 entries * 16) - 1 = 4095
    %% Base: IDTBaseAddr

    %% For simplicity, we'll store the IDTR in a known location
    %% Let's use the address right after the IDT: IDTBaseAddr + 4096
    IDTRAddr = IDTBaseAddr + 4096,

    Limit = 4095,

    iolist_to_binary([
        %% Build IDTR in memory
        %% mov rax, IDTRAddr
        encode_mov_imm64(rax, IDTRAddr),

        %% mov word [rax], Limit
        encode_mov_mem_imm16(rax, 0, Limit),

        %% mov rbx, IDTBaseAddr
        encode_mov_imm64(rbx, IDTBaseAddr),

        %% mov [rax+2], rbx  (store IDT base)
        encode_mov_mem_store(rax, 2, rbx),

        %% lidt [rax]
        encode_lidt({rax, 0}),

        %% ret
        encode_ret()
    ]).

%%====================================================================
%% ISR Stubs
%%====================================================================

%% @doc Returns x86_64 machine code for ISR stubs (34 stubs total).
%%      - Stubs 0-31: Exception handlers (some CPU-provided error codes)
%%      - Stub 32: Timer ISR (calls separate timer handler)
%%      - Stub 33: Generic stub (for unused interrupts, just halts)
%%
%%      For the hackathon, the common handler writes exception info to
%%      serial port (COM1 0x3F8) and halts.
-spec exception_stubs() -> binary().
exception_stubs() ->
    %% Exceptions with CPU-provided error codes: 8, 10-14, 17, 21
    %% Others need to push a dummy error code for stack alignment
    ErrorCodeExceptions = [8, 10, 11, 12, 13, 14, 17, 21],

    %% Build stub for each exception 0-31
    ExceptionStubs = [build_exception_stub(N, lists:member(N, ErrorCodeExceptions)) || N <- lists:seq(0, 31)],

    %% Stub 32: Timer ISR
    TimerStub = build_timer_stub(),

    %% Stub 33: Generic halt stub
    GenericStub = build_generic_stub(),

    %% Common handler (called by all exception stubs)
    CommonHandler = build_common_handler(),

    iolist_to_binary([ExceptionStubs, TimerStub, GenericStub, CommonHandler]).

%% @doc Build a single exception stub.
%%      Each stub is exactly 10 bytes:
%%      - push dummy/nothing (0-2 bytes if no error code)
%%      - push exception number (2 bytes)
%%      - jmp common_handler (5 bytes relative)
%%
%%      If CPU provides error code, we don't push dummy.
%%      Total must be 10 bytes, so we pad with nop if needed.
-spec build_exception_stub(non_neg_integer(), boolean()) -> binary().
build_exception_stub(ExcNum, HasErrorCode) ->
    %% Common handler comes after: 32 exception stubs + timer stub + generic stub
    %% Exception stubs: 32 * 10 = 320 bytes
    %% Timer stub: variable size (measure it)
    TimerStubSize = byte_size(build_timer_stub()),
    %% Generic stub: variable size (measure it)
    GenericStubSize = byte_size(build_generic_stub()),

    %% Build stub first to know its actual size
    Base = case HasErrorCode of
        false ->
            %% CPU doesn't push error code, so we push dummy 0
            %% push 0 (2 bytes: 6A 00)
            [<<16#6A, 16#00>>];
        true ->
            %% CPU already pushed error code, don't push dummy
            []
    end,

    %% push exception number (2 bytes: 6A <num>)
    PushVector = <<16#6A, ExcNum:8>>,

    %% Calculate jmp displacement BEFORE building jmp instruction
    %% This stub starts at: ExcNum * 10
    %% The jmp instruction starts at: (ExcNum * 10) + byte_size(Base) + 2
    %% The jmp ends at (instruction starts after the jmp): jmp_start + 5
    %% Common handler starts at: 320 + TimerStubSize + GenericStubSize
    %% Displacement = target - (jmp_address + 5)
    ThisStubStart = ExcNum * 10,
    JmpInsnStart = ThisStubStart + iolist_size(Base) + 2,
    JmpInsnEnd = JmpInsnStart + 5,
    CommonHandlerStart = 320 + TimerStubSize + GenericStubSize,
    Displacement = CommonHandlerStart - JmpInsnEnd,

    %% jmp rel32 to common handler (5 bytes: E9 <offset>)
    JmpInsn = <<16#E9, Displacement:32/little-signed>>,

    Stub = iolist_to_binary([Base, PushVector, JmpInsn]),

    %% Pad to exactly 10 bytes
    PadSize = 10 - byte_size(Stub),
    Padding = binary:copy(<<16#90>>, PadSize),  %% nop

    <<Stub/binary, Padding/binary>>.

%% @doc Build timer ISR stub (stub 32).
%%      Calls the timer handler code (which is separate).
-spec build_timer_stub() -> binary().
build_timer_stub() ->
    %% For now, just increment a counter and iret
    %% We'll put the counter at a known memory location
    %% Let's use address 0x7000 (arbitrary low memory)
    CounterAddr = 16#7000,

    iolist_to_binary([
        %% push rax
        encode_push(rax),

        %% mov rax, CounterAddr
        encode_mov_imm64(rax, CounterAddr),

        %% inc qword [rax]
        encode_inc_mem64(rax, 0),

        %% Send EOI to PIC (out 0x20, 0x20)
        %% mov al, 0x20
        encode_mov_al_imm(16#20),
        %% out 0x20, al
        encode_outb_imm(16#20),

        %% pop rax
        encode_pop(rax),

        %% iretq
        encode_iretq()
    ]).

%% @doc Build generic stub for unused interrupts.
-spec build_generic_stub() -> binary().
build_generic_stub() ->
    iolist_to_binary([
        %% hlt
        encode_hlt(),
        %% jmp -2 (infinite loop)
        <<16#EB, 16#FE>>
    ]).

%% @doc Build common exception handler.
%%      For hackathon: output exception number to serial and halt.
-spec build_common_handler() -> binary().
build_common_handler() ->
    iolist_to_binary([
        %% Stack layout at entry:
        %%   [rsp+0]  = exception number (pushed by stub)
        %%   [rsp+8]  = error code (CPU or dummy)
        %%   [rsp+16] = RIP
        %%   [rsp+24] = CS
        %%   [rsp+32] = RFLAGS
        %%   [rsp+40] = RSP
        %%   [rsp+48] = SS

        %% Save registers
        encode_push(rax),
        encode_push(rbx),
        encode_push(rcx),
        encode_push(rdx),

        %% Load exception number from stack
        %% mov rax, [rsp+32]  (skip 4 pushed regs)
        encode_mov_mem_load(rax, rsp, 32),

        %% Convert exception number to ASCII hex digit
        %% For simplicity, just add '0' or 'A'
        %% add al, '0'
        encode_add_al_imm(16#30),

        %% Output to serial (COM1 0x3F8)
        %% Wait for TX ready
        %% mov edx, 0x3FD (LSR)
        encode_mov_edx_imm(16#3FD),

        %% wait_tx_ready:
        %% in al, dx
        encode_inb_dx(),
        %% test al, 0x20
        encode_test_al_imm(16#20),
        %% jz wait_tx_ready (loop if not ready, offset -7)
        <<16#74, 16#F6>>,

        %% mov edx, 0x3F8 (THR)
        encode_mov_edx_imm(16#3F8),
        %% out dx, al
        encode_outb_dx(),

        %% Halt forever
        encode_hlt(),
        %% jmp -2
        <<16#EB, 16#FE>>
    ]).

%% timer_isr/0 removed - functionality is in build_timer_stub/0
%% which is called by exception_stubs/0

%%====================================================================
%% x86_64 Instruction Encoders
%%====================================================================

%% Import from vbeam_native_x86_64 where available, or define locally

encode_mov_imm64(Reg, Imm) ->
    Rex = rex(1, 0, 0, reg_hi(Reg)),
    Opcode = 16#B8 + reg_lo(Reg),
    <<Rex:8, Opcode:8, Imm:64/little-signed>>.

encode_mov_mem_load(Dst, Base, Offset) ->
    encode_mem_op(16#8B, Dst, Base, Offset).

encode_mov_mem_store(Base, Offset, Src) ->
    encode_mem_op(16#89, Src, Base, Offset).

encode_mov_mem_imm16(Base, Offset, Imm) ->
    %% mov word [Base+Offset], Imm
    %% Encoding: 66 [REX] C7 /0 iw
    %% ModR/M for memory indirect:
    %%   mod=00 (no disp) + rm=base → [base]
    %%   mod=01 (disp8) + rm=base → [base+disp8]
    %%   mod=10 (disp32) + rm=base → [base+disp32]
    %%   EXCEPT: rbp/r13 (rm=101) with mod=00 means disp32, so use mod=01 disp8=0
    %%   EXCEPT: rsp/r12 (rm=100) requires SIB byte
    Rex = rex(0, 0, 0, reg_hi(Base)),
    BaseRM = reg_lo(Base),

    case {Offset, BaseRM} of
        {0, 5} ->  %% rbp/r13 special case: mod=00 means disp32, use mod=01 disp8=0
            ModRM = modrm(2#01, 0, BaseRM),
            <<16#66, Rex:8, 16#C7, ModRM:8, 0:8, Imm:16/little>>;
        {0, 4} ->  %% rsp/r12 special case: need SIB
            ModRM = modrm(2#00, 0, 2#100),
            SIB = 16#24,  %% base=rsp, index=none
            <<16#66, Rex:8, 16#C7, ModRM:8, SIB:8, Imm:16/little>>;
        {0, _} ->  %% Normal case: [base], mod=00
            ModRM = modrm(2#00, 0, BaseRM),
            <<16#66, Rex:8, 16#C7, ModRM:8, Imm:16/little>>;
        {Disp, 4} when Disp >= -128, Disp =< 127 ->  %% rsp/r12 with disp8
            ModRM = modrm(2#01, 0, 2#100),
            SIB = 16#24,
            <<16#66, Rex:8, 16#C7, ModRM:8, SIB:8, Disp:8/little-signed, Imm:16/little>>;
        {Disp, _} when Disp >= -128, Disp =< 127 ->  %% disp8
            ModRM = modrm(2#01, 0, BaseRM),
            <<16#66, Rex:8, 16#C7, ModRM:8, Disp:8/little-signed, Imm:16/little>>;
        {Disp, 4} ->  %% rsp/r12 with disp32
            ModRM = modrm(2#10, 0, 2#100),
            SIB = 16#24,
            <<16#66, Rex:8, 16#C7, ModRM:8, SIB:8, Disp:32/little-signed, Imm:16/little>>;
        {Disp, _} ->  %% disp32
            ModRM = modrm(2#10, 0, BaseRM),
            <<16#66, Rex:8, 16#C7, ModRM:8, Disp:32/little-signed, Imm:16/little>>
    end.

encode_lea_rip_rel(Dst, Offset) ->
    Rex = rex(1, reg_hi(Dst), 0, 0),
    ModRM = modrm(2#00, reg_lo(Dst), 2#101),
    <<Rex:8, 16#8D:8, ModRM:8, Offset:32/little-signed>>.

encode_lgdt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 2, Base, Offset).

encode_lidt({Base, Offset}) ->
    encode_mem_op_ext(16#0F, 16#01, 3, Base, Offset).

encode_push(Reg) ->
    Opcode = 16#50 + reg_lo(Reg),
    case needs_rex(Reg) of
        true ->
            Rex = rex(0, 0, 0, 1),
            <<Rex:8, Opcode:8>>;
        false ->
            <<Opcode:8>>
    end.

encode_push_imm32(Imm) ->
    %% push imm32: 68 id
    <<16#68, Imm:32/little-signed>>.

encode_pop(Reg) ->
    Opcode = 16#58 + reg_lo(Reg),
    case needs_rex(Reg) of
        true ->
            Rex = rex(0, 0, 0, 1),
            <<Rex:8, Opcode:8>>;
        false ->
            <<Opcode:8>>
    end.

encode_ret() ->
    <<16#C3>>.

encode_retfq() ->
    %% Far return: 48 CB
    <<16#48, 16#CB>>.

encode_mov_ax_imm16(Imm) ->
    %% mov ax, imm16: 66 B8 iw
    <<16#66, 16#B8, Imm:16/little>>.

encode_mov_ds_ax() ->
    %% mov ds, ax: 8E D8
    <<16#8E, 16#D8>>.

encode_mov_es_ax() ->
    %% mov es, ax: 8E C0
    <<16#8E, 16#C0>>.

encode_mov_fs_ax() ->
    %% mov fs, ax: 8E E0
    <<16#8E, 16#E0>>.

encode_mov_gs_ax() ->
    %% mov gs, ax: 8E E8
    <<16#8E, 16#E8>>.

encode_mov_ss_ax() ->
    %% mov ss, ax: 8E D0
    <<16#8E, 16#D0>>.

encode_mov_al_imm(Imm) ->
    %% mov al, imm8: B0 ib
    <<16#B0, Imm:8>>.

encode_mov_edx_imm(Imm) ->
    %% mov edx, imm32: BA id
    <<16#BA, Imm:32/little>>.

encode_add_al_imm(Imm) ->
    %% add al, imm8: 04 ib
    <<16#04, Imm:8>>.

encode_test_al_imm(Imm) ->
    %% test al, imm8: A8 ib
    <<16#A8, Imm:8>>.

encode_inc_mem64(Base, Offset) ->
    %% inc qword [Base+Offset]: REX.W FF /0
    Rex = rex(1, 0, 0, reg_hi(Base)),
    BaseRM = reg_lo(Base),
    case {Offset, BaseRM} of
        %% rbp/r13 (rm=5) at offset 0 requires disp8 to avoid meaning disp32
        {0, 5} ->
            ModRM = modrm(2#01, 0, BaseRM),
            <<Rex:8, 16#FF, ModRM:8, 0:8>>;
        %% rsp/r12 (rm=4) requires SIB byte
        {0, 4} ->
            ModRM = modrm(2#00, 0, 2#100),
            SIB = 16#24,  %% base=rsp/r12, index=none
            <<Rex:8, 16#FF, ModRM:8, SIB:8>>;
        {0, _} ->
            ModRM = modrm(2#00, 0, BaseRM),
            <<Rex:8, 16#FF, ModRM:8>>;
        %% rsp/r12 with disp8
        {Disp, 4} when Disp >= -128, Disp =< 127 ->
            ModRM = modrm(2#01, 0, 2#100),
            SIB = 16#24,
            <<Rex:8, 16#FF, ModRM:8, SIB:8, Disp:8/little-signed>>;
        %% Other registers with disp8
        {Disp, _} when Disp >= -128, Disp =< 127 ->
            ModRM = modrm(2#01, 0, BaseRM),
            <<Rex:8, 16#FF, ModRM:8, Disp:8/little-signed>>;
        %% rsp/r12 with disp32
        {Disp, 4} ->
            ModRM = modrm(2#10, 0, 2#100),
            SIB = 16#24,
            <<Rex:8, 16#FF, ModRM:8, SIB:8, Disp:32/little-signed>>;
        %% Other registers with disp32
        {Disp, _} ->
            ModRM = modrm(2#10, 0, BaseRM),
            <<Rex:8, 16#FF, ModRM:8, Disp:32/little-signed>>
    end.

encode_inb_dx() ->
    <<16#EC>>.

encode_outb_dx() ->
    <<16#EE>>.

encode_outb_imm(Port) ->
    <<16#E6, Port:8>>.

encode_hlt() ->
    <<16#F4>>.

encode_iretq() ->
    <<16#48, 16#CF>>.

%%====================================================================
%% Register encoding helpers
%%====================================================================

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
reg_code(r15) -> 15.

reg_lo(Reg) ->
    reg_code(Reg) band 7.

reg_hi(Reg) ->
    (reg_code(Reg) bsr 3) band 1.

needs_rex(Reg) ->
    reg_code(Reg) >= 8.

rex(W, R, X, B) ->
    16#40 bor (W bsl 3) bor (R bsl 2) bor (X bsl 1) bor B.

modrm(Mod, Reg, RM) ->
    (Mod bsl 6) bor (Reg bsl 3) bor RM.

%%====================================================================
%% Memory addressing encoder
%%====================================================================

encode_mem_op(Opcode, RegField, Base, 0) when Base =/= rbp, Base =/= r13 ->
    Rex = rex(1, reg_hi(RegField), 0, reg_hi(Base)),
    case is_rsp_base(Base) of
        true ->
            ModRM = modrm(2#00, reg_lo(RegField), 2#100),
            SIB = 16#24,
            <<Rex:8, Opcode:8, ModRM:8, SIB:8>>;
        false ->
            ModRM = modrm(2#00, reg_lo(RegField), reg_lo(Base)),
            <<Rex:8, Opcode:8, ModRM:8>>
    end;
encode_mem_op(Opcode, RegField, Base, Offset) when Offset >= -128, Offset =< 127 ->
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

encode_mem_op_ext(Prefix, ExtOpcode, RegField, Base, 0) when Base =/= rbp, Base =/= r13 ->
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

is_rsp_base(rsp) -> true;
is_rsp_base(r12) -> true;
is_rsp_base(_)   -> false.
