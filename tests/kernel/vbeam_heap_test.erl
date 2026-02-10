%%% @doc Tests for vbeam_heap - BEAM process heap allocator
-module(vbeam_heap_test).
-export([run_all/0, test/0]).

%% Wrapper for make test-kernel compatibility
test() -> run_all().

%% Test runner
run_all() ->
    io:format("~n=== Running vbeam_heap tests ===~n~n"),

    Tests = [
        fun test_new_heap_small/0,
        fun test_new_heap_medium/0,
        fun test_new_heap_large/0,
        fun test_alloc_words/0,
        fun test_heap_grows/0,
        fun test_heap_shrinks/0,
        fun test_free_heap/0,
        fun test_gc_triggers/0,
        fun test_registry_ops/0,
        fun test_registry_stats/0,
        fun test_out_of_memory/0,
        fun test_stats_accuracy/0,
        fun test_mark_used/0,
        fun test_multiple_heaps/0
    ],

    Results = [run_test(Test) || Test <- Tests],
    Passed = length([ok || {ok, _} <- Results]),
    Failed = length(Results) - Passed,

    io:format("~n=== Summary: ~p passed, ~p failed ===~n", [Passed, Failed]),

    case Failed of
        0 -> halt(0);
        _ -> halt(1)
    end.

run_test(TestFun) ->
    TestName = extract_test_name(TestFun),
    io:format("Running ~s... ", [TestName]),
    try
        TestFun(),
        io:format("✓~n"),
        {ok, TestName}
    catch
        error:{assertion_failed, Msg} ->
            io:format("✗~n  FAILED: ~s~n", [Msg]),
            {error, TestName};
        Class:Reason:Stack ->
            io:format("✗~n  ERROR: ~p:~p~n~p~n", [Class, Reason, Stack]),
            {error, TestName}
    end.

extract_test_name(Fun) ->
    Info = erlang:fun_info(Fun),
    {name, Name} = lists:keyfind(name, 1, Info),
    atom_to_list(Name).

%% Assertion helpers
assert_eq(Expected, Actual, Msg) ->
    case Expected =:= Actual of
        true -> ok;
        false -> error({assertion_failed, io_lib:format("~s: expected ~p, got ~p", [Msg, Expected, Actual])})
    end.

assert_true(Cond, Msg) ->
    case Cond of
        true -> ok;
        false -> error({assertion_failed, Msg})
    end.

%% Pattern matching check (simplified - just check tuple tag)
assert_match(Expected, Actual, Msg) when is_tuple(Expected), is_tuple(Actual) ->
    ExpTag = element(1, Expected),
    ActTag = element(1, Actual),
    case ExpTag =:= ActTag of
        true -> ok;
        false -> error({assertion_failed, io_lib:format("~s: expected tuple with tag ~p, got ~p", [Msg, ExpTag, Actual])})
    end;
assert_match(Expected, Actual, Msg) ->
    case Expected =:= Actual of
        true -> ok;
        false -> error({assertion_failed, io_lib:format("~s: expected ~p, got ~p", [Msg, Expected, Actual])})
    end.

%%% ----------------------------------------------------------------------------
%%% Test Cases
%%% ----------------------------------------------------------------------------

%% Test: Create small heap (1 page = 4KB)
test_new_heap_small() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),  %% 64MB
    {ok, Heap, _NewAlloc} = vbeam_heap:new_heap(PageAlloc, small),

    #{size := Size, used := Used, pages := Pages} = Heap,
    assert_eq(4096, Size, "Small heap should be 4096 bytes"),
    assert_eq(0, Used, "New heap should have 0 used bytes"),
    assert_eq(1, length(Pages), "Small heap should have 1 page").

%% Test: Create medium heap (4 pages = 16KB)
test_new_heap_medium() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _NewAlloc} = vbeam_heap:new_heap(PageAlloc, medium),

    #{size := Size, used := Used, pages := Pages} = Heap,
    assert_eq(16384, Size, "Medium heap should be 16384 bytes"),
    assert_eq(0, Used, "New heap should have 0 used bytes"),
    assert_eq(4, length(Pages), "Medium heap should have 4 pages").

%% Test: Create large heap (16 pages = 64KB)
test_new_heap_large() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _NewAlloc} = vbeam_heap:new_heap(PageAlloc, large),

    #{size := Size, used := Used, pages := Pages} = Heap,
    assert_eq(65536, Size, "Large heap should be 65536 bytes"),
    assert_eq(0, Used, "New heap should have 0 used bytes"),
    assert_eq(16, length(Pages), "Large heap should have 16 pages").

%% Test: Allocate words from heap
test_alloc_words() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _} = vbeam_heap:new_heap(PageAlloc, small),

    %% Allocate 10 words (80 bytes)
    {ok, Offset1, Heap1} = vbeam_heap:alloc(Heap, 10, PageAlloc),
    assert_eq(0, Offset1, "First allocation should start at offset 0"),
    assert_eq(80, maps:get(used, Heap1), "Used should be 80 bytes after allocation"),

    %% Allocate 20 more words (160 bytes)
    {ok, Offset2, Heap2} = vbeam_heap:alloc(Heap1, 20, PageAlloc),
    assert_eq(80, Offset2, "Second allocation should start at offset 80"),
    assert_eq(240, maps:get(used, Heap2), "Used should be 240 bytes after second allocation").

%% Test: Heap grows when needed
test_heap_grows() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, PageAlloc1} = vbeam_heap:new_heap(PageAlloc, small),  %% 4KB = 512 words

    %% Fill to >90% (need 461 words of 512)
    {ok, _, Heap1} = vbeam_heap:alloc(Heap, 461, PageAlloc1),

    %% Next allocation should trigger need_grow
    Result = vbeam_heap:alloc(Heap1, 100, PageAlloc1),
    assert_match({need_grow, ignored}, Result, "Allocation beyond capacity should trigger need_grow"),

    %% Actually grow the heap
    {ok, Heap2, _PageAlloc2} = vbeam_heap:grow_heap(Heap1, PageAlloc1),
    #{size := NewSize, pages := NewPages} = Heap2,
    assert_eq(8192, NewSize, "Heap should double to 8192 bytes"),
    assert_eq(2, length(NewPages), "Heap should have 2 pages after growth").

%% Test: Heap shrinks when mostly empty
test_heap_shrinks() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, PageAlloc1} = vbeam_heap:new_heap(PageAlloc, large),  %% 64KB

    %% Use only 1KB (12.5% of 8KB, triggers shrink at <25%)
    Heap1 = vbeam_heap:mark_used(Heap, 1024),

    assert_true(vbeam_heap:should_shrink(Heap1), "Heap should need shrinking at <25% usage"),

    %% Shrink the heap
    {ok, Heap2, _PageAlloc2} = vbeam_heap:shrink_heap(Heap1, PageAlloc1),
    #{size := NewSize, pages := NewPages} = Heap2,
    assert_eq(32768, NewSize, "Heap should halve to 32768 bytes"),
    assert_eq(8, length(NewPages), "Heap should have 8 pages after shrink").

%% Test: Free heap returns pages
test_free_heap() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, PageAlloc1} = vbeam_heap:new_heap(PageAlloc, medium),

    StatsBefore = vbeam_page_alloc:stats(PageAlloc1),
    PageAlloc2 = vbeam_heap:free_heap(Heap, PageAlloc1),
    StatsAfter = vbeam_page_alloc:stats(PageAlloc2),

    FreeBefore = maps:get(free, StatsBefore),
    FreeAfter = maps:get(free, StatsAfter),

    assert_eq(FreeBefore + 4, FreeAfter, "Free count should increase by 4 pages").

%% Test: GC triggers at correct thresholds
test_gc_triggers() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _} = vbeam_heap:new_heap(PageAlloc, small),  %% 4KB

    %% At 0% usage
    assert_true(not vbeam_heap:should_gc(Heap), "Should not GC at 0% usage"),

    %% At 50% usage
    Heap50 = vbeam_heap:mark_used(Heap, 2048),
    assert_true(not vbeam_heap:should_gc(Heap50), "Should not GC at 50% usage"),

    %% At 76% usage (above 75% threshold)
    Heap76 = vbeam_heap:mark_used(Heap, 3113),
    assert_true(vbeam_heap:should_gc(Heap76), "Should GC at 76% usage"),

    %% At 91% usage (above 90% threshold)
    Heap91 = vbeam_heap:mark_used(Heap, 3729),
    assert_true(vbeam_heap:should_gc(Heap91), "Should GC at 91% usage"),
    assert_true(vbeam_heap:should_grow(Heap91), "Should grow at 91% usage").

%% Test: Registry operations
test_registry_ops() ->
    Registry = vbeam_heap:registry_new(),
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _} = vbeam_heap:new_heap(PageAlloc, small),

    %% Add heap
    Pid = self(),
    Registry1 = vbeam_heap:registry_add(Registry, Pid, Heap),

    %% Retrieve heap
    {ok, RetrievedHeap} = vbeam_heap:registry_get(Registry1, Pid),
    assert_true(maps:get(id, RetrievedHeap) > 0, "Heap should have an ID"),

    %% Remove heap
    Registry2 = vbeam_heap:registry_remove(Registry1, Pid),
    Result = vbeam_heap:registry_get(Registry2, Pid),
    assert_match({error, not_found}, Result, "Removed heap should not be found").

%% Test: Registry stats aggregate correctly
test_registry_stats() ->
    Registry = vbeam_heap:registry_new(),
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),

    %% Add 3 heaps
    {ok, Heap1, PA1} = vbeam_heap:new_heap(PageAlloc, small),
    {ok, Heap2, PA2} = vbeam_heap:new_heap(PA1, medium),
    {ok, Heap3, _} = vbeam_heap:new_heap(PA2, large),

    Heap1Used = vbeam_heap:mark_used(Heap1, 1000),
    Heap2Used = vbeam_heap:mark_used(Heap2, 5000),
    Heap3Used = vbeam_heap:mark_used(Heap3, 20000),

    Pid1 = list_to_pid("<0.1.0>"),
    Pid2 = list_to_pid("<0.2.0>"),
    Pid3 = list_to_pid("<0.3.0>"),

    Registry1 = vbeam_heap:registry_add(Registry, Pid1, Heap1Used),
    Registry2 = vbeam_heap:registry_add(Registry1, Pid2, Heap2Used),
    Registry3 = vbeam_heap:registry_add(Registry2, Pid3, Heap3Used),

    %% Get aggregate stats
    Stats = vbeam_heap:registry_stats(Registry3),
    #{total := Total, used := Used, count := Count} = Stats,

    assert_eq(4096 + 16384 + 65536, Total, "Total should sum all heaps"),
    assert_eq(1000 + 5000 + 20000, Used, "Used should sum all heaps"),
    assert_eq(3, Count, "Count should be 3 heaps").

%% Test: Out of memory handling
test_out_of_memory() ->
    %% Create allocator with 2MB + 3 pages (2MB reserved + 3 usable)
    %% 2MB = 512 pages, so total = 515 pages = 2,109,440 bytes
    PageAlloc = vbeam_page_alloc:init(2109440),

    %% First allocation should succeed (1 page)
    {ok, _Heap1, PageAlloc1} = vbeam_heap:new_heap(PageAlloc, small),

    %% Second allocation should succeed (1 page)
    {ok, _Heap2, PageAlloc2} = vbeam_heap:new_heap(PageAlloc1, small),

    %% Third allocation should succeed (1 page)
    {ok, _Heap3, PageAlloc3} = vbeam_heap:new_heap(PageAlloc2, small),

    %% Fourth allocation should fail (out of memory)
    %% Note: vbeam_page_alloc returns {{error, Reason}, State} on failure
    case vbeam_heap:new_heap(PageAlloc3, small) of
        {{error, _Reason}, _State} -> ok;  %% Expected
        {error, _Reason} -> ok;             %% Also acceptable
        Other -> error({assertion_failed, io_lib:format("Expected error, got ~p", [Other])})
    end.

%% Test: Stats accuracy
test_stats_accuracy() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _} = vbeam_heap:new_heap(PageAlloc, medium),

    Heap1 = vbeam_heap:mark_used(Heap, 8192),  %% Use 50% of 16KB

    Stats = vbeam_heap:stats(Heap1),
    #{total := Total, used := Used, free := Free, pages := Pages, fragmentation := Frag} = Stats,

    assert_eq(16384, Total, "Total should be 16384"),
    assert_eq(8192, Used, "Used should be 8192"),
    assert_eq(8192, Free, "Free should be 8192"),
    assert_eq(4, Pages, "Pages should be 4"),
    assert_true(abs(Frag - 0.5) < 0.01, "Fragmentation should be ~0.5").

%% Test: mark_used updates counter
test_mark_used() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),
    {ok, Heap, _} = vbeam_heap:new_heap(PageAlloc, small),

    assert_eq(0, maps:get(used, Heap), "Initial used should be 0"),

    Heap1 = vbeam_heap:mark_used(Heap, 2048),
    assert_eq(2048, maps:get(used, Heap1), "Used should be 2048 after mark"),

    Heap2 = vbeam_heap:mark_used(Heap1, 1024),
    assert_eq(1024, maps:get(used, Heap2), "Used should be 1024 after second mark (replaces, not adds)").

%% Test: Multiple heaps can coexist
test_multiple_heaps() ->
    PageAlloc = vbeam_page_alloc:init(64 * 1024 * 1024),

    %% Create multiple heaps
    {ok, Heap1, PA1} = vbeam_heap:new_heap(PageAlloc, small),
    {ok, Heap2, PA2} = vbeam_heap:new_heap(PA1, medium),
    {ok, Heap3, PA3} = vbeam_heap:new_heap(PA2, large),

    %% All should have different page addresses
    Pages1 = maps:get(pages, Heap1),
    Pages2 = maps:get(pages, Heap2),
    Pages3 = maps:get(pages, Heap3),

    %% Check no overlap
    AllPages = Pages1 ++ Pages2 ++ Pages3,
    UniquePages = lists:usort(AllPages),
    assert_eq(length(AllPages), length(UniquePages), "All pages should be unique"),

    %% Free all heaps
    PA4 = vbeam_heap:free_heap(Heap1, PA3),
    PA5 = vbeam_heap:free_heap(Heap2, PA4),
    _PA6 = vbeam_heap:free_heap(Heap3, PA5),

    ok.
