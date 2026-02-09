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

    iolist_to_binary([
        %% === Load GDT ===
        vbeam_gdt_idt:gdt_load_code(GDTBase),

        %% === Load IDT ===
        vbeam_gdt_idt:idt_load_code(IDTBase),

        %% === Load CR3 (page tables) ===
        vbeam_paging:load_cr3_code(PageTablesBase),

        %% === Setup stack ===
        %% mov rsp, StackBase + StackSize
        encode_mov_rsp_imm64(StackBase + StackSize),

        %% Halt loop
        <<16#F4>>,       %% hlt
        <<16#EB, 16#FD>> %% jmp -3 (infinite loop)
    ]).

%% @doc Generate boot data section containing GDT, IDT, page tables, and strings.
-spec boot_data(map()) -> binary().
boot_data(Config) ->
    #{
        gdt_base := _GDTBase,
        idt_base := _IDTBase,
        page_tables_base := _PageTablesBase
    } = Config,

    %% GDT data (includes 5 entries + GDTR)
    GDTData = vbeam_gdt_idt:gdt_data(),

    %% IDT data (256 entries)
    %% ISR stubs will be at known location - pass base address
    ISRStubsBase = 16#210000,  %% Placeholder, actual location TBD
    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),

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

    iolist_to_binary([GDTData, IDTData, PageTablesData, Strings]).

%% @doc Returns layout map describing data section structure.
-spec boot_data_layout(map()) -> map().
boot_data_layout(Config) ->
    #{
        gdt_base := _GDTBase,
        idt_base := _IDTBase
    } = Config,

    %% Calculate sizes
    GDTData = vbeam_gdt_idt:gdt_data(),
    GDTSize = byte_size(GDTData),

    ISRStubsBase = 16#210000,
    IDTData = vbeam_gdt_idt:idt_data(ISRStubsBase),
    IDTSize = byte_size(IDTData),

    PageTablesData = vbeam_paging:page_tables(4),
    PageTablesSize = byte_size(PageTablesData),

    %% String data
    Strings = [
        <<"[BOOT] ExitBootServices...\r\n", 0>>,
        <<"[BOOT] GDT loaded\r\n", 0>>,
        <<"[BOOT] IDT loaded\r\n", 0>>,
        <<"[BOOT] Paging configured (4GB identity-mapped)\r\n", 0>>,
        <<"[BOOT] Stack ready\r\n", 0>>,
        <<"[BOOT] BEAM kernel ready\r\n", 0>>
    ],

    %% Calculate offsets
    GDTOffset = 0,
    IDTOffset = GDTOffset + GDTSize,
    PageTablesOffset = IDTOffset + IDTSize,
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

%% @doc Build serial print helper code.
%%      This is the same as vbeam_nucleus_boot:build_print_string/0
%%      Input: RDI = null-terminated string pointer
%%      Outputs each character to COM1 (0x3F8)
-spec build_serial_print() -> binary().
build_serial_print() ->
    iolist_to_binary([
        <<16#48, 16#89, 16#FE>>,                %% mov rsi, rdi
        <<16#AC>>,                               %% lodsb
        <<16#84, 16#C0>>,                        %% test al, al
        <<16#74, 16#16>>,                        %% jz done (22 bytes ahead)
        <<16#88, 16#C3>>,                        %% mov bl, al (save char)
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3FD (LSR)
        <<16#EC>>,                               %% in al, dx
        <<16#A8, 16#20>>,                        %% test al, 0x20 (TX ready?)
        <<16#74, 16#F6>>,                        %% jnz wait_loop
        <<16#88, 16#D8>>,                        %% mov al, bl (restore char)
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3F8 (THR)
        <<16#EE>>,                               %% out dx, al
        <<16#EB, 16#E5>>,                        %% jmp next_char
        <<16#C3>>                                %% ret
    ]).

%% @doc Generate call to serial_print_code with RIP-relative lea.
%%      PrintFuncOffset: offset of print function from start of code
%%      StringOffsets: map of string label -> offset
%%      StringLabel: which string to print
%%      CurrentOffset: current position in code
-spec serial_print_call(non_neg_integer(), map(), atom(), non_neg_integer()) -> binary().
serial_print_call(PrintFuncOffset, StringOffsets, StringLabel, CurrentOffset) ->
    StringOffset = maps:get(StringLabel, StringOffsets),

    %% lea rdi, [rip + offset_to_string]
    %% Offset is: StringOffset - (CurrentOffset + InstructionSize)
    %% lea rdi is 7 bytes, call is 5 bytes
    LeaSize = 7,
    CallSize = 5,

    %% Position after lea instruction
    AfterLea = CurrentOffset + LeaSize,
    RIPRelStringOffset = StringOffset - AfterLea,

    %% Position after call instruction
    AfterCall = AfterLea + CallSize,
    CallOffset = PrintFuncOffset - AfterCall,

    iolist_to_binary([
        %% lea rdi, [rip + RIPRelStringOffset]
        <<16#48, 16#8D, 16#3D, RIPRelStringOffset:32/little-signed>>,

        %% call print_string
        <<16#E8, CallOffset:32/little-signed>>
    ]).

%% @doc Find offset of a string in the string list.
-spec find_string_offset(binary(), [{non_neg_integer(), binary()}]) -> non_neg_integer().
find_string_offset(String, [{Offset, Str} | Rest]) ->
    case Str of
        String -> Offset;
        _ -> find_string_offset(String, Rest)
    end;
find_string_offset(String, []) ->
    error({string_not_found, String}).

%% @doc Build list of {Offset, String} tuples.
-spec build_string_offsets([binary()], non_neg_integer()) -> [{non_neg_integer(), binary()}].
build_string_offsets(Strings, BaseOffset) ->
    build_string_offsets(Strings, BaseOffset, []).

build_string_offsets([], _CurrentOffset, Acc) ->
    lists:reverse(Acc);
build_string_offsets([Str | Rest], CurrentOffset, Acc) ->
    build_string_offsets(Rest, CurrentOffset + byte_size(Str),
                        [{CurrentOffset, Str} | Acc]).

%% @doc Encode: mov rsp, imm64
-spec encode_mov_rsp_imm64(non_neg_integer()) -> binary().
encode_mov_rsp_imm64(Imm) ->
    %% REX.W + opcode 0xBC (mov rsp) + imm64
    %% rsp = register 4, so REX.B = 0
    Rex = 16#48,  %% REX.W
    Opcode = 16#BC,  %% mov r12 (base opcode for rsp in REX encoding)
    <<Rex:8, Opcode:8, Imm:64/little>>.
