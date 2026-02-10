%%% @doc Test suite for vbeam_page_alloc
%%%
%%% Tests physical page-frame allocator functionality.
%%% Runs on standard Erlang (not bare metal) for development.
%%%
-module(vbeam_page_alloc_test).
-export([run/0, test/0]).

%% Wrapper for make test-kernel compatibility
test() -> run().

-define(MB, 1024 * 1024).
-define(PAGE_SIZE, 4096).

run() ->
    io:format("~n=== Page Allocator Test Suite ===~n~n"),

    test_basic_init(),
    test_single_alloc_free(),
    test_multiple_alloc(),
    test_contiguous_alloc(),
    test_mark_reserved(),
    test_mark_reserved_reverse_range(),
    test_exhaustion(),
    test_fragmentation(),
    test_edge_cases(),

    io:format("~n=== All Tests Passed ===~n"),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test Cases
%%% ----------------------------------------------------------------------------

test_basic_init() ->
    io:format("Test: Basic initialization... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),
    Stats0 = vbeam_page_alloc:stats(State0),

    %% Total pages = 16MB / 4KB = 4096 pages
    4096 = maps:get(total, Stats0),

    %% First 2MB reserved = 512 pages
    %% Free = 4096 - 512 = 3584
    3584 = maps:get(free, Stats0),
    512 = maps:get(allocated, Stats0),

    io:format("PASS~n"),
    ok.

test_single_alloc_free() ->
    io:format("Test: Single page alloc/free... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),

    %% Allocate one page
    {ok, Addr1, State1} = vbeam_page_alloc:alloc_page(State0),
    true = Addr1 >= 0,
    true = (Addr1 rem ?PAGE_SIZE) =:= 0, %% Must be page-aligned

    Stats1 = vbeam_page_alloc:stats(State1),
    3583 = maps:get(free, Stats1), %% One less free
    513 = maps:get(allocated, Stats1),

    %% Free the page
    State2 = vbeam_page_alloc:free_page(State1, Addr1),
    Stats2 = vbeam_page_alloc:stats(State2),
    3584 = maps:get(free, Stats2), %% Back to original free count
    512 = maps:get(allocated, Stats2),

    %% Allocate again - should get same or different page
    {ok, Addr2, _State3} = vbeam_page_alloc:alloc_page(State2),
    true = Addr2 >= 0,

    io:format("PASS~n"),
    ok.

test_multiple_alloc() ->
    io:format("Test: Multiple page allocation... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),

    %% Allocate 100 pages (not necessarily contiguous)
    {ok, Addrs, State1} = vbeam_page_alloc:alloc_pages(State0, 100),
    100 = length(Addrs),

    %% All addresses should be unique and page-aligned
    100 = length(lists:usort(Addrs)),
    lists:foreach(fun(Addr) ->
        true = (Addr rem ?PAGE_SIZE) =:= 0
    end, Addrs),

    Stats1 = vbeam_page_alloc:stats(State1),
    3484 = maps:get(free, Stats1), %% 3584 - 100
    612 = maps:get(allocated, Stats1), %% 512 + 100

    %% Free all pages
    State2 = vbeam_page_alloc:free_pages(State1, Addrs),
    Stats2 = vbeam_page_alloc:stats(State2),
    3584 = maps:get(free, Stats2),

    io:format("PASS~n"),
    ok.

test_contiguous_alloc() ->
    io:format("Test: Contiguous allocation... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),

    %% Allocate 16 contiguous pages (64KB)
    {ok, Start1, State1} = vbeam_page_alloc:alloc_contiguous(State0, 16),
    true = (Start1 rem ?PAGE_SIZE) =:= 0,

    Stats1 = vbeam_page_alloc:stats(State1),
    3568 = maps:get(free, Stats1), %% 3584 - 16

    %% Allocate another contiguous block
    {ok, Start2, State2} = vbeam_page_alloc:alloc_contiguous(State1, 32),
    true = (Start2 rem ?PAGE_SIZE) =:= 0,

    %% The two blocks should not overlap
    true = (Start2 >= Start1 + (16 * ?PAGE_SIZE)) orelse
           (Start1 >= Start2 + (32 * ?PAGE_SIZE)),

    Stats2 = vbeam_page_alloc:stats(State2),
    3536 = maps:get(free, Stats2), %% 3568 - 32

    %% Free the blocks
    State3 = vbeam_page_alloc:free_page(State2, Start1),
    State4 = lists:foldl(fun(I, S) ->
        vbeam_page_alloc:free_page(S, Start1 + I * ?PAGE_SIZE)
    end, State3, lists:seq(1, 15)),

    State5 = lists:foldl(fun(I, S) ->
        vbeam_page_alloc:free_page(S, Start2 + I * ?PAGE_SIZE)
    end, State4, lists:seq(0, 31)),

    Stats5 = vbeam_page_alloc:stats(State5),
    3584 = maps:get(free, Stats5),

    io:format("PASS~n"),
    ok.

test_mark_reserved() ->
    io:format("Test: Mark reserved regions... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),

    %% Reserve 14MB-16MB (last 2MB)
    ReserveStart = 14 * ?MB,
    ReserveEnd = 16 * ?MB - 1,
    State1 = vbeam_page_alloc:mark_reserved(State0, ReserveStart, ReserveEnd),

    Stats1 = vbeam_page_alloc:stats(State1),
    %% 3584 - 512 pages (2MB) = 3072 free
    3072 = maps:get(free, Stats1),
    1024 = maps:get(allocated, Stats1), %% 512 + 512

    %% Try to allocate - should not get addresses in reserved range
    {ok, Addr, _State2} = vbeam_page_alloc:alloc_page(State1),
    true = Addr < ReserveStart,

    io:format("PASS~n"),
    ok.

test_mark_reserved_reverse_range() ->
    io:format("Test: Mark reserved with reverse range (StartAddr > EndAddr)... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),
    Stats0 = vbeam_page_alloc:stats(State0),

    %% Try to reserve with StartAddr > EndAddr (invalid range)
    State1 = vbeam_page_alloc:mark_reserved(State0, 8 * ?MB, 4 * ?MB),

    %% Stats should be unchanged - no crash, no negative page count
    Stats1 = vbeam_page_alloc:stats(State1),
    Stats0 = Stats1,

    io:format("PASS~n"),
    ok.

test_exhaustion() ->
    io:format("Test: Memory exhaustion... "),
    %% Small memory pool for easier exhaustion
    State0 = vbeam_page_alloc:init(1 * ?MB), %% 256 pages total

    %% First 2MB reserved, but we only have 1MB total
    %% So first 256 pages are all reserved
    Stats0 = vbeam_page_alloc:stats(State0),

    %% With 1MB total (256 pages), the init reserves 2MB worth (512 pages)
    %% But since we only have 256 total, it will try to reserve 0..511
    %% This tests edge case where reserved > total
    256 = maps:get(total, Stats0),

    %% For the main exhaustion test, use larger memory
    State1 = vbeam_page_alloc:init(4 * ?MB), %% 1024 pages
    Stats1 = vbeam_page_alloc:stats(State1),
    512 = maps:get(free, Stats1), %% 1024 - 512

    %% Allocate all free pages
    {ok, Addrs, State2} = vbeam_page_alloc:alloc_pages(State1, 512),
    512 = length(Addrs),

    Stats2 = vbeam_page_alloc:stats(State2),
    0 = maps:get(free, Stats2),

    %% Try to allocate when exhausted
    {error, out_of_memory} = vbeam_page_alloc:alloc_page(State2),
    {error, out_of_memory} = vbeam_page_alloc:alloc_contiguous(State2, 1),

    io:format("PASS~n"),
    ok.

test_fragmentation() ->
    io:format("Test: Fragmentation handling... "),
    State0 = vbeam_page_alloc:init(16 * ?MB),

    %% Allocate many small blocks
    {ok, Addrs1, State1} = vbeam_page_alloc:alloc_pages(State0, 100),

    %% Free every other page to create fragmentation
    ToFree = [lists:nth(I, Addrs1) || I <- lists:seq(1, 100), I rem 2 =:= 0],
    State2 = vbeam_page_alloc:free_pages(State1, ToFree),

    Stats2 = vbeam_page_alloc:stats(State2),
    3534 = maps:get(free, Stats2), %% 3584 - 100 + 50

    %% Should still be able to allocate contiguous block in unfragmented region
    {ok, _Start, State3} = vbeam_page_alloc:alloc_contiguous(State2, 64),

    Stats3 = vbeam_page_alloc:stats(State3),
    3470 = maps:get(free, Stats3), %% 3534 - 64

    io:format("PASS~n"),
    ok.

test_edge_cases() ->
    io:format("Test: Edge cases... "),

    %% Minimum memory
    State0 = vbeam_page_alloc:init(?PAGE_SIZE),
    Stats0 = vbeam_page_alloc:stats(State0),
    1 = maps:get(total, Stats0),

    %% Try to allocate contiguous block larger than available
    State1 = vbeam_page_alloc:init(16 * ?MB),
    {error, _} = vbeam_page_alloc:alloc_contiguous(State1, 5000), %% More than total

    %% Allocate exactly all free pages contiguously
    {ok, _Start, State2} = vbeam_page_alloc:alloc_contiguous(State1, 3584),
    Stats2 = vbeam_page_alloc:stats(State2),
    0 = maps:get(free, Stats2),

    %% Double free (should be idempotent - no accounting change)
    State3 = vbeam_page_alloc:init(16 * ?MB),
    {ok, Addr, State4} = vbeam_page_alloc:alloc_page(State3),
    State5 = vbeam_page_alloc:free_page(State4, Addr),
    Stats5 = vbeam_page_alloc:stats(State5),
    State6 = vbeam_page_alloc:free_page(State5, Addr), %% Double free
    Stats6 = vbeam_page_alloc:stats(State6),
    %% Stats should be exactly the same - double-free should not change accounting
    Stats5 = Stats6,

    io:format("PASS~n"),
    ok.
