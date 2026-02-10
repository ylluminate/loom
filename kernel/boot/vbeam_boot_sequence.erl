%%% @doc Boot sequence orchestrator for Loom OS.
%%% Generates x86_64 machine code that orchestrates the complete boot sequence:
%%%   1. ExitBootServices (placeholder for now)
%%%   2. GDT setup and load
%%%   3. IDT setup and load
%%%   4. Paging configuration (4-level, 4GB identity-mapped)
%%%   5. Stack setup
%%%   6. BEAM kernel readiness
%%% @end
-module(vbeam_boot_sequence).

-export([
    boot_code/1,
    boot_data/1,
    boot_data_layout/1
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Generate complete boot sequence code.
%%      Takes config map with physical addresses for each component.
%%      Returns x86_64 machine code binary that:
%%        - Prints status messages to serial (COM1)
%%        - Loads GDT, IDT, page tables
%%        - Sets up kernel stack
%%        - Halts in a loop
%%
%%      For now, simplified to just return a basic boot sequence
%%      without serial output (serial_print_call is complex due to
%%      RIP-relative addressing and we can add it later).
%%
%% CODEX R38 FINDING #5 FIX: Spec can return error tuple from paging module
-spec boot_code(map()) -> binary() | {error, term()}.
boot_code(Config) ->
    #{
        gdt_base := GDTBase,
        idt_base := IDTBase,
        page_tables_base := PageTablesBase,
        stack_base := StackBase
    } = Config,

    %% Stack size (assume 4KB stack for now)
    StackSize = 16#1000,

    %% For now, just generate the core boot sequence without status prints
    %% Status prints can be added in a future iteration with proper
    %% RIP-relative addressing setup

    %% Get helper code blocks and strip trailing ret (0xC3) for inlining
    %% FINDING 4 FIX: Pattern-match to propagate errors from paging module
    GDTCode = vbeam_gdt_idt:gdt_load_code(GDTBase),
    IDTCode = vbeam_gdt_idt:idt_load_code(IDTBase),

    case vbeam_paging:load_cr3_code(PageTablesBase) of
        {error, _} = Err ->
            Err;
        PageCode when is_binary(PageCode) ->
            iolist_to_binary([
                %% === Disable interrupts during boot setup ===
                <<16#FA>>,                  %% cli (clear interrupt flag)

                %% === Load GDT ===
                strip_trailing_ret(GDTCode),

                %% === Load IDT ===
                strip_trailing_ret(IDTCode),

                %% === Load CR3 (page tables) ===
                strip_trailing_ret(PageCode),

        %% === Setup stack ===
        %% mov rsp, StackBase + StackSize
        encode_mov_rsp_imm64(StackBase + StackSize),

        %% === Initialize PIC (8259A) ===
        %% Remap IRQs to vectors 32-47 (avoid conflicts with exceptions 0-31)
        %% ICW1: Start initialization
        <<16#B0, 16#11>>,           %% mov al, 0x11 (ICW1: init + ICW4 needed)
        <<16#E6, 16#20>>,           %% out 0x20, al (master PIC command)
        <<16#E6, 16#A0>>,           %% out 0xA0, al (slave PIC command)
        %% ICW2: Vector offsets
        <<16#B0, 16#20>>,           %% mov al, 0x20 (master base vector)
        <<16#E6, 16#21>>,           %% out 0x21, al (master PIC data)
        <<16#B0, 16#28>>,           %% mov al, 0x28 (slave base vector)
        <<16#E6, 16#A1>>,           %% out 0xA1, al (slave PIC data)
        %% ICW3: Cascade
        <<16#B0, 16#04>>,           %% mov al, 0x04 (master: slave on IRQ2)
        <<16#E6, 16#21>>,           %% out 0x21, al
        <<16#B0, 16#02>>,           %% mov al, 0x02 (slave: cascade identity)
        <<16#E6, 16#A1>>,           %% out 0xA1, al
        %% ICW4: Mode
        <<16#B0, 16#01>>,           %% mov al, 0x01 (8086 mode)
        <<16#E6, 16#21>>,           %% out 0x21, al
        <<16#E6, 16#A1>>,           %% out 0xA1, al
        %% Unmask only IRQ 0 (timer) on both PICs (OCW1)
        <<16#B0, 16#FE>>,           %% mov al, 0xFE (unmask IRQ 0 only)
        <<16#E6, 16#21>>,           %% out 0x21, al (master mask)
        <<16#B0, 16#FF>>,           %% mov al, 0xFF (mask all on slave)
        <<16#E6, 16#A1>>,           %% out 0xA1, al (slave mask)

        %% === Initialize PIT (8254) for ~100Hz timer ===
        %% Channel 0, mode 2 (rate generator), binary
        <<16#B0, 16#34>>,           %% mov al, 0x34 (cmd: ch0, lobyte/hibyte, mode 2)
        <<16#E6, 16#43>>,           %% out 0x43, al (PIT command port)
        %% Divisor = 1193182 / 100 ≈ 11932 (0x2E9C)
        <<16#B0, 16#9C>>,           %% mov al, 0x9C (low byte)
        <<16#E6, 16#40>>,           %% out 0x40, al (ch0 data)
        <<16#B0, 16#2E>>,           %% mov al, 0x2E (high byte)
        <<16#E6, 16#40>>,           %% out 0x40, al

        %% === Enable interrupts ===
        <<16#FB>>,                  %% sti

                %% === Idle loop (halt with interrupts enabled) ===
                <<16#F4>>,                  %% hlt
                <<16#EB, 16#FD>>            %% jmp -3 (infinite loop: hlt again after interrupt)
            ])
    end.

%% @doc Generate boot data section containing GDT, IDT, IDTR, ISR stubs, page tables, and strings.
%%      Data is packed sequentially but must align with configured base addresses.
%%      IDTR is placed at IDTBase + 4096 to match idt_load_code expectations.
-spec boot_data(map()) -> binary().
boot_data(Config) ->
    #{
        gdt_base := GDTBase,
        idt_base := IDTBase,
        page_tables_base := PageTablesBase
    } = Config,

    %% Get ISR stubs base from config or compute default
    ISRStubsBase = maps:get(isr_stubs_base, Config, PageTablesBase + 16#100000),

    %% FINDING 9 FIX: Validate both ordering AND non-overlap with actual sizes
    case GDTBase >= 0 andalso GDTBase =< 16#4000000 of
        true -> ok;
        false -> error({invalid_boot_config, {gdt_base_out_of_range, GDTBase}})
    end,

    %% Calculate actual data sizes to validate no overlap
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTActualSize = byte_size(GDTData),
    GDTEnd = GDTBase + GDTActualSize,

    case IDTBase >= GDTEnd andalso IDTBase =< 16#4000000 of
        true -> ok;
        false -> error({regions_overlap, gdt, {GDTBase, GDTEnd}, idt, IDTBase})
    end,

    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTActualSize = byte_size(IDTData) + 10,  % IDT + IDTR
    IDTEnd = IDTBase + IDTActualSize,

    case PageTablesBase >= IDTEnd andalso PageTablesBase =< 16#4000000 of
        true -> ok;
        false -> error({regions_overlap, idt, {IDTBase, IDTEnd}, page_tables, PageTablesBase})
    end,

    PageTablesData = vbeam_paging:page_tables(PageTablesBase, 4),
    PageTablesActualSize = byte_size(PageTablesData),
    PageTablesEnd = PageTablesBase + PageTablesActualSize,

    case ISRStubsBase >= PageTablesEnd andalso ISRStubsBase =< 16#4000000 of
        true -> ok;
        false -> error({regions_overlap, page_tables, {PageTablesBase, PageTablesEnd}, isr_stubs, ISRStubsBase})
    end,

    %% GDT data (includes 5 entries + GDTR at end)
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTSize = byte_size(GDTData),

    %% Padding between GDT and IDT if needed
    ExpectedIDTOffset = IDTBase - GDTBase,
    GDTPaddingSize = if ExpectedIDTOffset > GDTSize -> ExpectedIDTOffset - GDTSize; true -> 0 end,
    %% Padding bounded by validated config addresses above
    GDTPadding = if
        GDTPaddingSize > 0 ->
            binary:copy(<<0>>, GDTPaddingSize);
        true ->
            <<>>
    end,

    %% IDT data (256 entries, 4096 bytes)
    %% ISR stubs will be emitted after page tables
    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTSize = byte_size(IDTData),

    %% IDTR (10 bytes: 2-byte limit + 8-byte base)
    %% idt_load_code expects IDTR at IDTBase + 4096
    %% Reserve space for it
    IDTRReserved = <<0:80>>,  %% 10 bytes zeroed

    %% Padding between IDT+IDTR and page tables if needed
    ExpectedPageTablesOffset = PageTablesBase - GDTBase,
    ActualPageTablesOffset = GDTSize + byte_size(GDTPadding) + IDTSize + 10,
    PageTablesPaddingSize = if
        ExpectedPageTablesOffset > ActualPageTablesOffset ->
            ExpectedPageTablesOffset - ActualPageTablesOffset;
        true ->
            0
    end,
    %% Padding bounded by validated config addresses above
    PageTablesPadding = if
        PageTablesPaddingSize > 0 ->
            binary:copy(<<0>>, PageTablesPaddingSize);
        true ->
            <<>>
    end,

    %% Page tables (4GB identity-mapped) with absolute base addresses
    PageTablesData = vbeam_paging:page_tables(PageTablesBase, 4),

    %% ISR exception stubs (must be emitted at ISRStubsBase offset)
    %% Calculate padding needed to reach ISRStubsBase
    ActualISRStubsOffset = ISRStubsBase - GDTBase,
    CurrentOffset = GDTSize + byte_size(GDTPadding) + IDTSize + 10 + byte_size(PageTablesPadding) + byte_size(PageTablesData),
    ISRStubsPaddingSize = if
        ActualISRStubsOffset > CurrentOffset ->
            ActualISRStubsOffset - CurrentOffset;
        true ->
            0
    end,
    %% Padding bounded by validated config addresses above
    ISRStubsPadding = if
        ISRStubsPaddingSize > 0 ->
            binary:copy(<<0>>, ISRStubsPaddingSize);
        true ->
            <<>>
    end,
    ISRStubs = vbeam_gdt_idt:exception_stubs(),

    %% Status strings (null-terminated)
    Strings = [
        <<"[BOOT] ExitBootServices...\r\n", 0>>,
        <<"[BOOT] GDT loaded\r\n", 0>>,
        <<"[BOOT] IDT loaded\r\n", 0>>,
        <<"[BOOT] Paging configured (4GB identity-mapped)\r\n", 0>>,
        <<"[BOOT] Stack ready\r\n", 0>>,
        <<"[BOOT] BEAM kernel ready\r\n", 0>>
    ],

    iolist_to_binary([GDTData, GDTPadding, IDTData, IDTRReserved, PageTablesPadding, PageTablesData, ISRStubsPadding, ISRStubs, Strings]).

%% @doc Returns layout map describing data section structure.
%%      Must match the structure created by boot_data/1.
-spec boot_data_layout(map()) -> map().
boot_data_layout(Config) ->
    #{
        gdt_base := GDTBase,
        idt_base := IDTBase,
        page_tables_base := PageTablesBase
    } = Config,

    %% Get ISR stubs base from config or compute default
    ISRStubsBase = maps:get(isr_stubs_base, Config, PageTablesBase + 16#100000),

    %% Calculate sizes (must match boot_data/1)
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTSize = byte_size(GDTData),

    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTSize = byte_size(IDTData),

    PageTablesData = vbeam_paging:page_tables(PageTablesBase, 4),
    PageTablesSize = byte_size(PageTablesData),

    ISRStubs = vbeam_gdt_idt:exception_stubs(),
    ISRStubsSize = byte_size(ISRStubs),

    %% Calculate padding (same logic as boot_data/1)
    ExpectedIDTOffset = IDTBase - GDTBase,
    GDTPaddingSize = if
        ExpectedIDTOffset > GDTSize -> ExpectedIDTOffset - GDTSize;
        true -> 0
    end,

    ExpectedPageTablesOffset = PageTablesBase - GDTBase,
    ActualPageTablesOffset = GDTSize + GDTPaddingSize + IDTSize + 10,  %% 10 = IDTR size
    PageTablesPaddingSize = if
        ExpectedPageTablesOffset > ActualPageTablesOffset ->
            ExpectedPageTablesOffset - ActualPageTablesOffset;
        true ->
            0
    end,

    ActualISRStubsOffset = ISRStubsBase - GDTBase,
    CurrentOffset = GDTSize + GDTPaddingSize + IDTSize + 10 + PageTablesPaddingSize + PageTablesSize,
    ISRStubsPaddingSize = if
        ActualISRStubsOffset > CurrentOffset ->
            ActualISRStubsOffset - CurrentOffset;
        true ->
            0
    end,

    %% String data
    Strings = [
        <<"[BOOT] ExitBootServices...\r\n", 0>>,
        <<"[BOOT] GDT loaded\r\n", 0>>,
        <<"[BOOT] IDT loaded\r\n", 0>>,
        <<"[BOOT] Paging configured (4GB identity-mapped)\r\n", 0>>,
        <<"[BOOT] Stack ready\r\n", 0>>,
        <<"[BOOT] BEAM kernel ready\r\n", 0>>
    ],

    %% Calculate actual offsets in the packed data
    GDTOffset = 0,
    IDTOffset = GDTOffset + GDTSize + GDTPaddingSize,
    PageTablesOffset = IDTOffset + IDTSize + 10 + PageTablesPaddingSize,  %% 10 = IDTR
    ISRStubsOffset = PageTablesOffset + PageTablesSize + ISRStubsPaddingSize,
    StringsOffset = ISRStubsOffset + ISRStubsSize,

    %% Build string offset list
    StringOffsets = build_string_offsets(Strings, StringsOffset),

    #{
        gdt_offset => GDTOffset,
        gdt_size => GDTSize,
        idt_offset => IDTOffset,
        idt_size => IDTSize,
        page_tables_offset => PageTablesOffset,
        page_tables_size => PageTablesSize,
        isr_stubs_offset => ISRStubsOffset,
        isr_stubs_size => ISRStubsSize,
        strings => StringOffsets
    }.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% Dead functions removed - unused helpers for future serial print implementation
%% build_serial_print/0, serial_print_call/4, find_string_offset/2 were not used

%% @doc Build list of {Offset, String} tuples.
-spec build_string_offsets([binary()], non_neg_integer()) -> [{non_neg_integer(), binary()}].
build_string_offsets(Strings, BaseOffset) ->
    build_string_offsets(Strings, BaseOffset, []).

build_string_offsets([], _CurrentOffset, Acc) ->
    lists:reverse(Acc);
build_string_offsets([Str | Rest], CurrentOffset, Acc) ->
    build_string_offsets(Rest, CurrentOffset + byte_size(Str),
                        [{CurrentOffset, Str} | Acc]).

%% @doc Strip trailing ret (0xC3) from code block for inlining.
-spec strip_trailing_ret(binary()) -> binary().
strip_trailing_ret(<<>>) ->
    <<>>;
strip_trailing_ret(Code) ->
    Size = byte_size(Code),
    case binary:last(Code) of
        16#C3 ->
            %% Strip the ret
            binary:part(Code, 0, Size - 1);
        _ ->
            %% No ret, return as-is
            Code
    end.

%% @doc Encode: mov rsp, imm64
-spec encode_mov_rsp_imm64(non_neg_integer()) -> binary().
encode_mov_rsp_imm64(Imm) ->
    %% REX.W + opcode 0xBC (mov rsp) + imm64
    %% rsp = register 4, so REX.B = 0
    Rex = 16#48,  %% REX.W
    Opcode = 16#BC,  %% mov r12 (base opcode for rsp in REX encoding)
    <<Rex:8, Opcode:8, Imm:64/little>>.
