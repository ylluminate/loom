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
-spec boot_code(map()) -> binary().
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
    GDTCode = vbeam_gdt_idt:gdt_load_code(GDTBase),
    IDTCode = vbeam_gdt_idt:idt_load_code(IDTBase),
    PageCode = vbeam_paging:load_cr3_code(PageTablesBase),

    iolist_to_binary([
        %% === Load GDT ===
        strip_trailing_ret(GDTCode),

        %% === Load IDT ===
        strip_trailing_ret(IDTCode),

        %% === Load CR3 (page tables) ===
        strip_trailing_ret(PageCode),

        %% === Setup stack ===
        %% mov rsp, StackBase + StackSize
        encode_mov_rsp_imm64(StackBase + StackSize),

        %% Halt loop
        <<16#F4>>,       %% hlt
        <<16#EB, 16#FD>> %% jmp -3 (infinite loop)
    ]).

%% @doc Generate boot data section containing GDT, IDT, IDTR, page tables, and strings.
%%      Data is packed sequentially but must align with configured base addresses.
%%      IDTR is placed at IDTBase + 4096 to match idt_load_code expectations.
-spec boot_data(map()) -> binary().
boot_data(Config) ->
    #{
        gdt_base := GDTBase,
        idt_base := IDTBase,
        page_tables_base := PageTablesBase
    } = Config,

    %% GDT data (includes 5 entries + GDTR at end)
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTSize = byte_size(GDTData),

    %% Padding between GDT and IDT if needed
    ExpectedIDTOffset = IDTBase - GDTBase,
    GDTPadding = if
        ExpectedIDTOffset > GDTSize ->
            binary:copy(<<0>>, ExpectedIDTOffset - GDTSize);
        true ->
            <<>>
    end,

    %% IDT data (256 entries, 4096 bytes)
    %% ISR stubs will be at known location - pass base address
    ISRStubsBase = 16#210000,  %% Placeholder, actual location TBD
    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTSize = byte_size(IDTData),

    %% IDTR (10 bytes: 2-byte limit + 8-byte base)
    %% idt_load_code expects IDTR at IDTBase + 4096
    %% Reserve space for it
    IDTRReserved = <<0:80>>,  %% 10 bytes zeroed

    %% Padding between IDT+IDTR and page tables if needed
    ExpectedPageTablesOffset = PageTablesBase - GDTBase,
    ActualPageTablesOffset = GDTSize + byte_size(GDTPadding) + IDTSize + 10,

    PageTablesPadding = if
        ExpectedPageTablesOffset > ActualPageTablesOffset ->
            binary:copy(<<0>>, ExpectedPageTablesOffset - ActualPageTablesOffset);
        true ->
            <<>>
    end,

    %% Page tables (4GB identity-mapped)
    PageTablesData = vbeam_paging:page_tables(4),

    %% Status strings (null-terminated)
    Strings = [
        <<"[BOOT] ExitBootServices...\r\n", 0>>,
        <<"[BOOT] GDT loaded\r\n", 0>>,
        <<"[BOOT] IDT loaded\r\n", 0>>,
        <<"[BOOT] Paging configured (4GB identity-mapped)\r\n", 0>>,
        <<"[BOOT] Stack ready\r\n", 0>>,
        <<"[BOOT] BEAM kernel ready\r\n", 0>>
    ],

    iolist_to_binary([GDTData, GDTPadding, IDTData, IDTRReserved, PageTablesPadding, PageTablesData, Strings]).

%% @doc Returns layout map describing data section structure.
%%      Must match the structure created by boot_data/1.
-spec boot_data_layout(map()) -> map().
boot_data_layout(Config) ->
    #{
        gdt_base := GDTBase,
        idt_base := IDTBase,
        page_tables_base := PageTablesBase
    } = Config,

    %% Calculate sizes (must match boot_data/1)
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTSize = byte_size(GDTData),

    ISRStubsBase = 16#210000,
    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTSize = byte_size(IDTData),

    PageTablesData = vbeam_paging:page_tables(4),
    PageTablesSize = byte_size(PageTablesData),

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
    StringsOffset = PageTablesOffset + PageTablesSize,

    %% Build string offset list
    StringOffsets = build_string_offsets(Strings, StringsOffset),

    #{
        gdt_offset => GDTOffset,
        gdt_size => GDTSize,
        idt_offset => IDTOffset,
        idt_size => IDTSize,
        page_tables_offset => PageTablesOffset,
        page_tables_size => PageTablesSize,
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
