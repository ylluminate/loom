%%% @doc BEAM Process Heap Allocator
%%%
%%% Allocates and manages BEAM process heaps on top of physical page allocator.
%%% Each BEAM process gets its own heap, allocated from physical pages.
%%%
%%% Design:
%%% - Heaps are collections of physical pages (4KB each)
%%% - Size hints: small (1 page), medium (4 pages), large (16 pages)
%%% - Heaps can grow (double) or shrink (halve) dynamically
%%% - Simple GC thresholds: >75% triggers GC, >90% triggers grow, <25% triggers shrink
%%% - Registry tracks all process heaps by process ID
%%%
%%% NO OTP dependencies - pure Erlang for bare metal.
%%%
-module(vbeam_heap).
-export([
    %% Heap lifecycle
    new_heap/2,
    free_heap/2,
    grow_heap/2,
    shrink_heap/2,

    %% Heap operations
    alloc/3,
    mark_used/2,
    stats/1,
    heaps_summary/1,

    %% GC helpers
    should_gc/1,
    should_grow/1,
    should_shrink/1,

    %% Registry
    registry_new/0,
    registry_add/3,
    registry_remove/2,
    registry_get/2,
    registry_stats/1
]).

-define(PAGE_SIZE, 4096).
-define(WORD_SIZE, 8).  %% 64-bit words

%% Size hints in pages
-define(SIZE_SMALL, 1).     %% 4KB = 1 page = 512 words
-define(SIZE_MEDIUM, 4).    %% 16KB = 4 pages = 2048 words
-define(SIZE_LARGE, 16).    %% 64KB = 16 pages = 8192 words

%% GC thresholds
-define(GC_THRESHOLD, 0.75).     %% Trigger GC at 75% full
-define(GROW_THRESHOLD, 0.90).   %% Grow heap at 90% full
-define(SHRINK_THRESHOLD, 0.25). %% Shrink heap at 25% full

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

-type heap() :: #{
    id => heap_id(),
    pages => [phys_addr()],
    size => non_neg_integer(),    %% Total bytes
    used => non_neg_integer(),    %% Used bytes
    generation => 0 | 1           %% Young (0) or old (1)
}.

-type heap_id() :: non_neg_integer().
-type phys_addr() :: non_neg_integer().
-type size_hint() :: small | medium | large.
-type alloc_state() :: term().  %% vbeam_page_alloc state
-type proc_id() :: non_neg_integer().

-type registry() :: #{
    heaps => #{proc_id() => heap()},
    next_id => heap_id()
}.

%%% ----------------------------------------------------------------------------
%%% Heap Lifecycle
%%% ----------------------------------------------------------------------------

%% @doc Allocate a new heap with size hint
-spec new_heap(alloc_state(), size_hint()) ->
    {ok, heap(), alloc_state()} | {error, out_of_memory}.
new_heap(PageAllocState, SizeHint) ->
    NumPages = size_hint_to_pages(SizeHint),
    case vbeam_page_alloc:alloc_pages(PageAllocState, NumPages) of
        {ok, Pages, NewPageAllocState} ->
            Heap = #{
                id => make_heap_id(),
                pages => Pages,
                size => NumPages * ?PAGE_SIZE,
                used => 0,
                generation => 0
            },
            {ok, Heap, NewPageAllocState};
        {Error, FailState} ->
            {Error, FailState}
    end.

%% @doc Free all pages belonging to a heap
-spec free_heap(heap(), alloc_state()) -> alloc_state().
free_heap(#{pages := Pages}, PageAllocState) ->
    vbeam_page_alloc:free_pages(PageAllocState, Pages).

%% @doc Double the heap size by allocating more pages
-spec grow_heap(heap(), alloc_state()) ->
    {ok, heap(), alloc_state()} | {error, term()}.
grow_heap(#{pages := OldPages, size := OldSize} = Heap, PageAllocState) ->
    NumPages = length_bare(OldPages),
    case vbeam_page_alloc:alloc_pages(PageAllocState, NumPages) of
        {ok, NewPages, NewPageAllocState} ->
            AllPages = append_bare(OldPages, NewPages),
            NewHeap = Heap#{
                pages => AllPages,
                size => OldSize * 2
            },
            {ok, NewHeap, NewPageAllocState};
        {Error, FailState} ->
            {Error, FailState}
    end.

%% @doc Halve the heap by freeing excess pages
-spec shrink_heap(heap(), alloc_state()) ->
    {ok, heap(), alloc_state()} | {error, term()}.
shrink_heap(#{pages := Pages, used := Used} = Heap, PageAllocState) ->
    NumPages = length_bare(Pages),
    case NumPages > 1 of
        true ->
            %% Can't shrink below used bytes
            MinPages = max_bare(1, (Used + ?PAGE_SIZE - 1) div ?PAGE_SIZE),
            TargetPages = max_bare(MinPages, NumPages div 2),

            %% Split pages: keep first TargetPages, free the rest
            {Keep, Free} = split_at_bare(TargetPages, Pages),

            NewPageAllocState = vbeam_page_alloc:free_pages(PageAllocState, Free),
            NewHeap = Heap#{
                pages => Keep,
                size => TargetPages * ?PAGE_SIZE
            },
            {ok, NewHeap, NewPageAllocState};
        false ->
            %% Can't shrink a single-page heap
            {ok, Heap, PageAllocState}
    end.

%%% ----------------------------------------------------------------------------
%%% Heap Operations
%%% ----------------------------------------------------------------------------

%% @doc Allocate N words from heap
%% Returns offset within heap, or triggers for GC/growth
-spec alloc(heap(), pos_integer(), alloc_state()) ->
    {ok, non_neg_integer(), heap()} |
    {need_gc, heap()} |
    {need_grow, heap()}.
alloc(#{size := Size, used := Used} = Heap, NumWords, _PageAllocState) ->
    NumBytes = NumWords * ?WORD_SIZE,
    NewUsed = Used + NumBytes,

    case NewUsed =< Size of
        true ->
            Offset = Used,
            NewHeap = Heap#{used => NewUsed},
            {ok, Offset, NewHeap};
        false ->
            %% Not enough space
            case should_grow(Heap) of
                true -> {need_grow, Heap};
                false -> {need_gc, Heap}
            end
    end.

%% @doc Update used counter after GC (marks new watermark)
-spec mark_used(heap(), non_neg_integer()) -> heap().
mark_used(Heap, UsedBytes) ->
    Heap#{used => UsedBytes}.

%% @doc Get heap statistics
-spec stats(heap()) -> #{atom() => term()}.
stats(#{size := Size, used := Used, pages := Pages, generation := Gen}) ->
    Free = Size - Used,
    Fragmentation = case Size of
        0 -> 0.0;
        _ -> (Used / Size)
    end,
    #{
        total => Size,
        used => Used,
        free => Free,
        pages => length_bare(Pages),
        fragmentation => Fragmentation,
        generation => Gen
    }.

%% @doc Aggregate statistics across multiple heaps
-spec heaps_summary([heap()]) -> #{atom() => term()}.
heaps_summary(Heaps) ->
    heaps_summary_loop(Heaps, #{
        total => 0,
        used => 0,
        free => 0,
        pages => 0,
        count => 0
    }).

heaps_summary_loop([], Acc) ->
    Acc;
heaps_summary_loop([Heap | Rest], Acc) ->
    #{total := Total, used := Used, free := Free, pages := Pages} = stats(Heap),
    NewAcc = #{
        total => maps:get(total, Acc) + Total,
        used => maps:get(used, Acc) + Used,
        free => maps:get(free, Acc) + Free,
        pages => maps:get(pages, Acc) + Pages,
        count => maps:get(count, Acc) + 1
    },
    heaps_summary_loop(Rest, NewAcc).

%%% ----------------------------------------------------------------------------
%%% GC Helpers
%%% ----------------------------------------------------------------------------

%% @doc Should trigger GC when heap is >75% full
-spec should_gc(heap()) -> boolean().
should_gc(#{size := 0}) -> false;
should_gc(#{size := Size, used := Used}) ->
    (Used / Size) > ?GC_THRESHOLD.

%% @doc Should grow when heap is >90% full
-spec should_grow(heap()) -> boolean().
should_grow(#{size := 0}) -> false;
should_grow(#{size := Size, used := Used}) ->
    (Used / Size) > ?GROW_THRESHOLD.

%% @doc Should shrink when heap is <25% full
-spec should_shrink(heap()) -> boolean().
should_shrink(#{size := Size, used := Used}) ->
    case Size > ?PAGE_SIZE of  %% Never shrink single-page heap
        true -> (Used / Size) < ?SHRINK_THRESHOLD;
        false -> false
    end.

%%% ----------------------------------------------------------------------------
%%% Registry (Track All Process Heaps)
%%% ----------------------------------------------------------------------------

%% @doc Create empty registry
-spec registry_new() -> registry().
registry_new() ->
    #{
        heaps => #{},
        next_id => 1
    }.

%% @doc Add heap to registry with process ID
-spec registry_add(registry(), proc_id(), heap()) -> registry().
registry_add(#{heaps := Heaps, next_id := NextId} = Registry, Pid, Heap) ->
    %% Assign ID if heap doesn't have one
    HeapWithId = case maps:get(id, Heap, undefined) of
        undefined -> Heap#{id => NextId};
        _ -> Heap
    end,

    NewHeaps = Heaps#{Pid => HeapWithId},
    Registry#{
        heaps => NewHeaps,
        next_id => NextId + 1
    }.

%% @doc Remove heap from registry by process ID
-spec registry_remove(registry(), proc_id()) -> registry().
registry_remove(#{heaps := Heaps} = Registry, Pid) ->
    NewHeaps = maps:remove(Pid, Heaps),
    Registry#{heaps => NewHeaps}.

%% @doc Get heap by process ID
-spec registry_get(registry(), proc_id()) -> {ok, heap()} | {error, not_found}.
registry_get(#{heaps := Heaps}, Pid) ->
    case maps:get(Pid, Heaps, undefined) of
        undefined -> {error, not_found};
        Heap -> {ok, Heap}
    end.

%% @doc Aggregate stats across all heaps in registry
-spec registry_stats(registry()) -> #{atom() => term()}.
registry_stats(#{heaps := Heaps}) ->
    HeapList = maps:values(Heaps),
    heaps_summary(HeapList).

%%% ----------------------------------------------------------------------------
%%% Internal Helpers
%%% ----------------------------------------------------------------------------

%% Convert size hint to number of pages
size_hint_to_pages(small) -> ?SIZE_SMALL;
size_hint_to_pages(medium) -> ?SIZE_MEDIUM;
size_hint_to_pages(large) -> ?SIZE_LARGE.

%% Generate unique heap ID (timestamp-based for bare metal)
make_heap_id() ->
    %% On bare metal, we'd use a counter or hardware timer
    %% For now, use erlang:unique_integer() equivalent
    erlang:system_time(nanosecond) band 16#FFFFFFFF.

%% Bare-metal list helpers (no lists:* imports for nucleus embedding)
length_bare(List) -> length_bare_loop(List, 0).
length_bare_loop([], Acc) -> Acc;
length_bare_loop([_ | Rest], Acc) -> length_bare_loop(Rest, Acc + 1).

append_bare([], L2) -> L2;
append_bare([H | T], L2) -> [H | append_bare(T, L2)].

split_at_bare(N, List) -> split_at_bare_loop(N, List, []).
split_at_bare_loop(0, Rest, Acc) -> {reverse_bare(Acc), Rest};
split_at_bare_loop(_, [], Acc) -> {reverse_bare(Acc), []};
split_at_bare_loop(N, [H | T], Acc) -> split_at_bare_loop(N - 1, T, [H | Acc]).

reverse_bare(List) -> reverse_bare_loop(List, []).
reverse_bare_loop([], Acc) -> Acc;
reverse_bare_loop([H | T], Acc) -> reverse_bare_loop(T, [H | Acc]).

%% Max function (avoid auto-import clash)
max_bare(A, B) when A > B -> A;
max_bare(_, B) -> B.
