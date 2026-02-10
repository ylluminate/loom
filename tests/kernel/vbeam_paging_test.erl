%%% @doc Tests for vbeam_paging module
-module(vbeam_paging_test).
-export([run_all/0, test/0]).

%% Wrapper for make test-kernel compatibility
test() -> run_all().

-define(PAGE_SIZE, 4096).
-define(ENTRIES_PER_TABLE, 512).

run_all() ->
    io:format("~n=== vbeam_paging Tests ===~n~n"),
    Tests = [
        fun test_page_tables_size/0,
        fun test_pml4_structure/0,
        fun test_pdpt_structure/0,
        fun test_pd_structure/0,
        fun test_identity_mapping/0,
        fun test_alignment/0,
        fun test_load_cr3_code/0,
        fun test_enable_paging_code/0,
        fun test_page_table_entry/0,
        fun test_2mb_pde_mask_correctness/0,
        fun test_2mb_alignment_required/0
    ],
    Results = [run_test(Test) || Test <- Tests],
    Failed = length([R || R <- Results, R =/= pass]),
    case Failed of
        0 ->
            io:format("~n✓ All tests passed!~n"),
            halt(0);
        N ->
            io:format("~n✗ ~w test(s) failed!~n", [N]),
            halt(1)
    end.

run_test(TestFun) ->
    TestName = atom_to_list(element(2, erlang:fun_info(TestFun, name))),
    io:format("  ~s ... ", [TestName]),
    try
        TestFun(),
        io:format("PASS~n"),
        pass
    catch
        Class:Reason:Stack ->
            io:format("FAIL~n    ~p:~p~n", [Class, Reason]),
            io:format("    Stack: ~p~n", [Stack]),
            fail
    end.

%%%----------------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------------

test_page_tables_size() ->
    %% 4GB = PML4 (4KB) + PDPT (4KB) + 4×PD (16KB) = 24KB
    Size4GB = vbeam_paging:page_tables_size(4),
    24576 = Size4GB,

    %% 1GB = PML4 (4KB) + PDPT (4KB) + 1×PD (4KB) = 12KB
    Size1GB = vbeam_paging:page_tables_size(1),
    12288 = Size1GB,

    %% 8GB = PML4 (4KB) + PDPT (4KB) + 8×PD (32KB) = 40KB
    Size8GB = vbeam_paging:page_tables_size(8),
    40960 = Size8GB,

    ok.

test_pml4_structure() ->
    Tables = vbeam_paging:page_tables(4),

    %% PML4 is the first 4KB
    <<PML4:?PAGE_SIZE/binary, _/binary>> = Tables,

    %% Entry 0 should be non-zero (points to PDPT)
    <<Entry0:64/little, RestEntries/binary>> = PML4,
    true = Entry0 =/= 0,

    %% Bits 0 (Present) and 1 (Writable) should be set
    1 = Entry0 band 16#01,  %% Present
    2 = Entry0 band 16#02,  %% Writable

    %% All other entries should be zero
    RestSize = (?ENTRIES_PER_TABLE - 1) * 8,
    <<RestEntries:RestSize/binary>> = RestEntries,
    true = is_all_zeros(RestEntries),

    ok.

test_pdpt_structure() ->
    Tables = vbeam_paging:page_tables(4),

    %% PDPT is the second 4KB
    <<_PML4:?PAGE_SIZE/binary, PDPT:?PAGE_SIZE/binary, _/binary>> = Tables,

    %% First 4 entries should be valid (for 4GB)
    <<Entry0:64/little, Entry1:64/little, Entry2:64/little, Entry3:64/little,
      RestEntries/binary>> = PDPT,

    %% All should be non-zero
    true = Entry0 =/= 0,
    true = Entry1 =/= 0,
    true = Entry2 =/= 0,
    true = Entry3 =/= 0,

    %% All should have Present + Writable
    lists:foreach(fun(Entry) ->
        1 = Entry band 16#01,
        2 = Entry band 16#02
    end, [Entry0, Entry1, Entry2, Entry3]),

    %% Rest should be zeros
    RestSize = (?ENTRIES_PER_TABLE - 4) * 8,
    <<RestEntries:RestSize/binary>> = RestEntries,
    true = is_all_zeros(RestEntries),

    ok.

test_pd_structure() ->
    Tables = vbeam_paging:page_tables(4),

    %% PD tables start at offset 8KB (after PML4 + PDPT)
    PDOffset = 2 * ?PAGE_SIZE,
    <<_:PDOffset/binary, PDTables/binary>> = Tables,

    %% Should have exactly 4 PD tables (4×4KB = 16KB)
    16384 = byte_size(PDTables),

    %% Each PD should have 512 entries
    <<PD0:?PAGE_SIZE/binary, _/binary>> = PDTables,
    ?PAGE_SIZE = byte_size(PD0),

    %% Every entry in PD0 should be valid (512 entries)
    verify_pd_entries(PD0, 0),

    ok.

test_identity_mapping() ->
    Tables = vbeam_paging:page_tables(4),

    %% Extract first PD (maps first 1GB)
    PDOffset = 2 * ?PAGE_SIZE,
    <<_:PDOffset/binary, PD0:?PAGE_SIZE/binary, _/binary>> = Tables,

    %% Entry 0 of PD0 should map physical address 0
    <<Entry0:64/little, _/binary>> = PD0,

    %% Extract physical address (bits 21-51 for 2MB pages)
    PhysAddr = Entry0 band 16#000FFFFFFFFFF000,
    0 = PhysAddr,  %% Should map to physical 0

    %% Should have PS bit set (bit 7)
    16#80 = Entry0 band 16#80,

    %% Check a few more entries
    <<_:8/binary, Entry1:64/little, _/binary>> = PD0,
    PhysAddr1 = Entry1 band 16#000FFFFFFFFFF000,
    TwoMB = 2 * 1024 * 1024,
    TwoMB = PhysAddr1,  %% Entry 1 should map to 2MB

    <<_:(2*8)/binary, Entry2:64/little, _/binary>> = PD0,
    PhysAddr2 = Entry2 band 16#000FFFFFFFFFF000,
    FourMB = 4 * 1024 * 1024,
    FourMB = PhysAddr2,  %% Entry 2 should map to 4MB

    ok.

test_alignment() ->
    %% All page table addresses in entries should be 4KB aligned
    Tables = vbeam_paging:page_tables(4),

    %% Check PML4 entry 0 (points to PDPT at offset 4096)
    <<Entry0:64/little, _/binary>> = Tables,
    Addr0 = Entry0 band 16#000FFFFFFFFFF000,
    0 = Addr0 rem ?PAGE_SIZE,  %% Should be 4KB aligned

    ok.

test_load_cr3_code() ->
    %% Test code generation for loading CR3
    TestAddr = 16#1000,  %% 4KB aligned
    Code = vbeam_paging:load_cr3_code(TestAddr),

    %% Should be exactly 12 bytes
    %% mov rax, imm64 (10 bytes) + mov cr3, rax (3 bytes) + ret (1 byte) = 14 bytes
    14 = byte_size(Code),

    %% Verify instruction sequence
    <<16#48, 16#B8,              %% mov rax, imm64
      TestAddr:64/little,         %% address
      16#0F, 16#22, 16#D8,       %% mov cr3, rax
      16#C3,                      %% ret
      _/binary>> = Code,

    ok.

test_enable_paging_code() ->
    Code = vbeam_paging:enable_paging_code(),

    %% Should be exactly 9 bytes
    %% mov rax, cr0 (3) + or eax, imm32 (5) + mov cr0, rax (3) + ret (1) = 12 bytes
    12 = byte_size(Code),

    %% Verify instruction sequence
    <<16#0F, 16#20, 16#C0,       %% mov rax, cr0
      16#0D, 16#00, 16#00, 16#01, 16#80,  %% or eax, 0x80010000
      16#0F, 16#22, 16#C0,       %% mov cr0, rax
      16#C3,                      %% ret
      _/binary>> = Code,

    ok.

test_page_table_entry() ->
    %% Test creating entries with different flags

    %% Simple PML4 entry: present + writable
    Entry1 = vbeam_paging:page_table_entry(16#1000, [present, writable], pml4),
    <<Val1:64/little>> = Entry1,
    16#1003 = Val1,  %% Address 0x1000 + flags 0x03

    %% PD entry with PS flag (2MB page)
    TwoMB = 2 * 1024 * 1024,
    Entry2 = vbeam_paging:page_table_entry(TwoMB, [present, writable, ps], pd),
    <<Val2:64/little>> = Entry2,
    %% Should have address + P + R/W + PS
    16#200083 = Val2,  %% 0x200000 + 0x01 + 0x02 + 0x80

    %% Test misaligned address (should error)
    try
        vbeam_paging:page_table_entry(16#1001, [present], pml4),
        error(should_have_failed)
    catch
        error:{misaligned_address, 16#1001} -> ok
    end,

    ok.

test_2mb_pde_mask_correctness() ->
    %% Test that 2MB page directory entries have bits 12-20 zeroed
    %% (these bits must be 0 for 2MB pages with PS flag)

    %% Create a 2MB-aligned address (2MB = 0x200000)
    TwoMB = 2 * 1024 * 1024,
    TestAddr = 16 * TwoMB,  %% 32MB, nicely aligned

    %% Create a PDE with PS flag
    Entry = vbeam_paging:page_table_entry(TestAddr, [present, writable, ps], pd),
    <<Val:64/little>> = Entry,

    %% Extract bits 12-20 (these should all be zero for 2MB pages)
    Bits12to20 = (Val bsr 12) band 16#1FF,  %% 9 bits starting at bit 12
    0 = Bits12to20,  %% Must be zero

    %% Verify the address is preserved in bits 21-51
    AddrMask = 16#000FFFFFFFFFF000,
    ExtractedAddr = Val band AddrMask,
    TestAddr = ExtractedAddr,

    ok.

test_2mb_alignment_required() ->
    %% Test 2MB alignment validation for PDE with PS flag
    %%
    %% CURRENT BEHAVIOR (as of 2026-02-10): The source has alignment check code
    %% (lines 116-118), but it's NOT ENFORCING properly - 1MB addresses pass.
    %% This test documents the expected behavior (should error) for TDD purposes.
    %%
    %% Once the alignment check is fixed, this test will pass.

    %% 1MB address (NOT 2MB aligned) - should be rejected
    OneMB = 1024 * 1024,
    TwoMB = 2 * 1024 * 1024,

    %% Verify our test address is truly misaligned
    true = (OneMB rem TwoMB) =/= 0,  %% 1MB is NOT 2MB-aligned

    %% Verify that misaligned 2MB pages are rejected
    %% NOTE: Can error with either misaligned_2mb_page OR address_bits_lost
    %%       Both are valid rejections of the bad address
    try
        vbeam_paging:page_table_entry(OneMB, [present, writable, ps], pd),
        error(should_have_failed_alignment_check)
    catch
        error:{misaligned_2mb_page, OneMB} -> ok;
        error:{address_bits_lost, OneMB, 0} -> ok
    end,

    %% Also test that properly aligned addresses DO work
    Entry = vbeam_paging:page_table_entry(TwoMB, [present, writable, ps], pd),
    <<Val:64/little>> = Entry,

    %% Verify it has the PS flag
    16#80 = Val band 16#80,

    ok.

%%%----------------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------------

%% @doc Check if binary is all zeros
is_all_zeros(Bin) ->
    is_all_zeros(Bin, 0).

is_all_zeros(<<>>, _) -> true;
is_all_zeros(<<0, Rest/binary>>, N) -> is_all_zeros(Rest, N+1);
is_all_zeros(_, _) -> false.

%% @doc Verify all entries in a PD table
verify_pd_entries(PD, _PDIndex) ->
    verify_pd_entries_loop(PD, 0).

verify_pd_entries_loop(<<>>, 512) -> ok;
verify_pd_entries_loop(<<Entry:64/little, Rest/binary>>, Index) ->
    %% Each entry should be non-zero
    true = Entry =/= 0,

    %% Should have Present + Writable + PS flags
    1 = Entry band 16#01,  %% Present
    2 = Entry band 16#02,  %% Writable
    16#80 = Entry band 16#80,  %% PS (2MB page)

    verify_pd_entries_loop(Rest, Index + 1).
