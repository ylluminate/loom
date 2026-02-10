%%% @doc Page table setup for x86_64 4-level paging (bare metal)
%%%
%%% Generates identity-mapped page table structures for Loom OS kernel.
%%% Uses 2MB pages for simplicity (no PT level needed).
%%%
%%% Memory layout:
%%%   PML4:  4KB (512 entries, only entry 0 used for 512GB coverage)
%%%   PDPT:  4KB (512 entries, N used for N GB)
%%%   PD:    4KB each (512 entries per GB, each entry = 2MB page)
%%%
%%% For 4GB: PML4 (4KB) + PDPT (4KB) + 4×PD (16KB) = 24KB total
%%%
%%% Page table entry format (64-bit):
%%%   Bits 12-51: Physical address (4KB aligned)
%%%   Bit 7: PS (Page Size) — 1 for 2MB pages in PD
%%%   Bit 2: U/S (User/Supervisor) — 0 for kernel
%%%   Bit 1: R/W (Read/Write) — 1 for writable
%%%   Bit 0: P (Present) — 1 for valid entry
%%%
%%% NO OTP dependencies - pure Erlang for bare metal code generation.
%%%
-module(vbeam_paging).
-export([
    page_tables/1,
    page_tables_size/1,
    load_cr3_code/1,
    enable_paging_code/0,
    page_table_entry/3
]).

-define(PAGE_SIZE, 4096).
-define(PAGE_2MB, 2 * 1024 * 1024).
-define(ENTRIES_PER_TABLE, 512).

%%% Flags
-define(FLAG_PRESENT,  16#01).
-define(FLAG_WRITABLE, 16#02).
-define(FLAG_USER,     16#04).
-define(FLAG_PS,       16#80).  %% Page Size bit (for 2MB pages)

%%%----------------------------------------------------------------------------
%%% Public API
%%%----------------------------------------------------------------------------

%% @doc Generate complete page table hierarchy for MaxPhysicalGB
%% Identity-mapped (virtual = physical) using 2MB pages
-spec page_tables(pos_integer()) -> binary().
page_tables(MaxPhysicalGB) when MaxPhysicalGB > 0 ->
    %% Calculate how many PD tables we need (one per GB)
    NumPDs = MaxPhysicalGB,

    %% Calculate base addresses (assume tables are laid out contiguously)
    %% Base is where we'll place these tables in physical memory
    %% For now, use offset 0 — caller will relocate
    PML4Base = 0,
    PDPTBase = PML4Base + ?PAGE_SIZE,
    PDBase   = PDPTBase + ?PAGE_SIZE,

    %% Build each level
    PML4 = build_pml4(PDPTBase),
    PDPT = build_pdpt(PDBase, NumPDs),
    PDs  = build_pds(NumPDs),

    %% Concatenate all tables
    iolist_to_binary([PML4, PDPT, PDs]).

%% @doc Calculate size of page tables for N GB
-spec page_tables_size(pos_integer()) -> non_neg_integer().
page_tables_size(MaxPhysicalGB) when MaxPhysicalGB > 0 ->
    NumPDs = MaxPhysicalGB,
    PML4Size = ?PAGE_SIZE,
    PDPTSize = ?PAGE_SIZE,
    PDsSize = NumPDs * ?PAGE_SIZE,
    PML4Size + PDPTSize + PDsSize.

%% @doc Generate x86_64 machine code to load CR3 with given address
%% Returns binary of machine code that:
%%   1. Moves address into RAX
%%   2. Loads CR3 from RAX
%%   3. Returns
-spec load_cr3_code(non_neg_integer()) -> binary().
load_cr3_code(PageTableAddr) ->
    iolist_to_binary([
        %% mov rax, imm64 (48 B8 followed by 8-byte address)
        <<16#48, 16#B8, PageTableAddr:64/little>>,
        %% mov cr3, rax (0F 22 D8)
        <<16#0F, 16#22, 16#D8>>,
        %% ret
        <<16#C3>>
    ]).

%% @doc Generate x86_64 machine code to enable/verify paging
%% Note: In long mode (UEFI), paging is already enabled.
%% This is for re-enabling after CR3 swap or verification.
-spec enable_paging_code() -> binary().
enable_paging_code() ->
    iolist_to_binary([
        %% mov rax, cr0 (0F 20 C0)
        <<16#0F, 16#20, 16#C0>>,
        %% or rax, 0x80010000 (PG bit 31 + WP bit 16)
        %% We use or eax, imm32 for smaller encoding
        <<16#0D, 16#00, 16#00, 16#01, 16#80>>,  %% or eax, 0x80010000
        %% mov cr0, rax (0F 22 C0)
        <<16#0F, 16#22, 16#C0>>,
        %% ret
        <<16#C3>>
    ]).

%% @doc Create a page table entry with given physical address, flags, and level
-spec page_table_entry(non_neg_integer(), [atom()], atom()) -> binary().
page_table_entry(PhysAddr, Flags, Level) ->
    %% Validate alignment: 2MB for 2MB pages, 4KB for others
    case {Level, lists:member(ps, Flags)} of
        {pd, true} ->
            %% 2MB page requires 2MB alignment
            case PhysAddr rem ?PAGE_2MB of
                0 -> ok;
                _ -> error({misaligned_2mb_page, PhysAddr})
            end;
        _ ->
            %% 4KB alignment for other entries
            case PhysAddr rem ?PAGE_SIZE of
                0 -> ok;
                _ -> error({misaligned_address, PhysAddr})
            end
    end,

    %% Convert flag atoms to bitmask
    FlagBits = flags_to_bits(Flags),

    %% For PD entries with PS flag, bits 12-20 must be zero (reserved)
    %% For 2MB pages, only bits 21-51 are valid for address
    AddrMask = case {Level, lists:member(ps, Flags)} of
        {pd, true} -> 16#000FFFFFFFE00000;  %% 2MB page: bits 21-51 (mask bits 12-20)
        _ -> 16#000FFFFFFFFFF000             %% 4KB page: bits 12-51
    end,

    Entry = (PhysAddr band AddrMask) bor FlagBits,
    <<Entry:64/little>>.

%%%----------------------------------------------------------------------------
%%% Internal Helpers
%%%----------------------------------------------------------------------------

%% @doc Build PML4 table (4KB, entry 0 points to PDPT, rest are zeros)
build_pml4(PDPTPhysAddr) ->
    %% Entry 0 points to PDPT
    Entry0 = page_table_entry(PDPTPhysAddr, [present, writable], pml4),

    %% Rest are zeros (511 empty entries)
    ZeroEntry = <<0:64/little>>,
    RestEntries = lists:duplicate(?ENTRIES_PER_TABLE - 1, ZeroEntry),

    iolist_to_binary([Entry0, RestEntries]).

%% @doc Build PDPT table (4KB, entries 0..N-1 point to PD tables)
build_pdpt(PDBaseAddr, NumPDs) ->
    %% Build valid entries (one per GB)
    ValidEntries = [
        page_table_entry(PDBaseAddr + (I * ?PAGE_SIZE), [present, writable], pdpt)
        || I <- lists:seq(0, NumPDs - 1)
    ],

    %% Rest are zeros
    NumZeros = ?ENTRIES_PER_TABLE - NumPDs,
    ZeroEntry = <<0:64/little>>,
    ZeroEntries = lists:duplicate(NumZeros, ZeroEntry),

    iolist_to_binary([ValidEntries, ZeroEntries]).

%% @doc Build all PD tables (each is 4KB, 512 entries mapping 2MB pages)
build_pds(NumPDs) ->
    PDs = [build_pd(GBIndex) || GBIndex <- lists:seq(0, NumPDs - 1)],
    iolist_to_binary(PDs).

%% @doc Build a single PD table for GBIndex (maps 1GB via 512×2MB pages)
build_pd(GBIndex) ->
    %% Each entry maps a 2MB page
    %% Physical address = (GBIndex * 1GB) + (EntryIndex * 2MB)
    GBBase = GBIndex * 1024 * 1024 * 1024,

    Entries = [
        page_table_entry(
            GBBase + (EntryIndex * ?PAGE_2MB),
            [present, writable, ps],
            pd
        )
        || EntryIndex <- lists:seq(0, ?ENTRIES_PER_TABLE - 1)
    ],

    iolist_to_binary(Entries).

%% @doc Convert flag atoms to bit flags
flags_to_bits(Flags) ->
    lists:foldl(fun flag_to_bit/2, 0, Flags).

flag_to_bit(present,  Acc) -> Acc bor ?FLAG_PRESENT;
flag_to_bit(writable, Acc) -> Acc bor ?FLAG_WRITABLE;
flag_to_bit(user,     Acc) -> Acc bor ?FLAG_USER;
flag_to_bit(ps,       Acc) -> Acc bor ?FLAG_PS;
flag_to_bit(_, Acc)        -> Acc.  %% Ignore unknown flags
