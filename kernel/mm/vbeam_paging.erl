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
    page_tables/2,
    page_tables_size/1,
    load_cr3_code/1,
    enable_paging_code/0,
    page_table_entry/3
]).

-define(PAGE_SIZE, 4096).
-define(PAGE_2MB, 2 * 1024 * 1024).
-define(ENTRIES_PER_TABLE, 512).

%%% Flags
-define(FLAG_PRESENT,       16#01).
-define(FLAG_WRITABLE,      16#02).
-define(FLAG_USER,          16#04).
-define(FLAG_WRITE_THROUGH, 16#08).  %% Bit 3
-define(FLAG_CACHE_DISABLE, 16#10).  %% Bit 4
-define(FLAG_ACCESSED,      16#20).  %% Bit 5
-define(FLAG_DIRTY,         16#40).  %% Bit 6
-define(FLAG_PS,            16#80).  %% Bit 7: Page Size (for 2MB pages)
-define(FLAG_GLOBAL,       16#100).  %% Bit 8
-define(FLAG_NX, 16#8000000000000000). %% Bit 63: No Execute

%%%----------------------------------------------------------------------------
%%% Public API
%%%----------------------------------------------------------------------------

%% @doc Generate complete page table hierarchy for MaxPhysicalGB
%% Identity-mapped (virtual = physical) using 2MB pages
%% Uses base address 0 (caller relocates if needed)
-spec page_tables(pos_integer()) -> binary().
page_tables(MaxPhysicalGB) when MaxPhysicalGB >= 1, MaxPhysicalGB =< 512 ->
    page_tables(0, MaxPhysicalGB).

%% @doc Generate page tables with explicit base address
%% All internal pointers are absolute (Base + offset)
-spec page_tables(non_neg_integer(), pos_integer()) -> binary().
page_tables(Base, MaxPhysicalGB) when MaxPhysicalGB >= 1, MaxPhysicalGB =< 512 ->
    %% Calculate how many PD tables we need (one per GB)
    NumPDs = MaxPhysicalGB,

    %% Calculate absolute addresses for each level
    PML4Base = Base,
    PDPTBase = PML4Base + ?PAGE_SIZE,
    PDBase   = PDPTBase + ?PAGE_SIZE,

    %% Build each level with absolute addresses
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
    %% Validate flags against known set
    KnownFlags = [present, writable, user, write_through, cache_disable, accessed, dirty, ps, global, nx],
    UnknownFlags = Flags -- KnownFlags,
    case UnknownFlags of
        [] -> ok;
        _ -> error({unknown_flags, UnknownFlags})
    end,

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

    %% Check if address masking would lose bits (address too large)
    MaskedAddr = PhysAddr band AddrMask,
    case MaskedAddr =:= PhysAddr of
        true -> ok;
        false -> error({address_bits_lost, PhysAddr, MaskedAddr})
    end,

    Entry = MaskedAddr bor FlagBits,
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

flag_to_bit(present,       Acc) -> Acc bor ?FLAG_PRESENT;
flag_to_bit(writable,      Acc) -> Acc bor ?FLAG_WRITABLE;
flag_to_bit(user,          Acc) -> Acc bor ?FLAG_USER;
flag_to_bit(write_through, Acc) -> Acc bor ?FLAG_WRITE_THROUGH;
flag_to_bit(cache_disable, Acc) -> Acc bor ?FLAG_CACHE_DISABLE;
flag_to_bit(accessed,      Acc) -> Acc bor ?FLAG_ACCESSED;
flag_to_bit(dirty,         Acc) -> Acc bor ?FLAG_DIRTY;
flag_to_bit(ps,            Acc) -> Acc bor ?FLAG_PS;
flag_to_bit(global,        Acc) -> Acc bor ?FLAG_GLOBAL;
flag_to_bit(nx,            Acc) -> Acc bor ?FLAG_NX;
flag_to_bit(_, Acc)             -> Acc.  %% Ignore unknown flags
