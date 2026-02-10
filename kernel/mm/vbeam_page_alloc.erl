%%% @doc Physical page-frame allocator for Loom OS
%%%
%%% Manages physical memory at 4KB page granularity using a bitmap.
%%% Supports up to 4GB physical memory (1M pages, 128KB bitmap).
%%%
%%% Design:
%%% - Bitmap where each bit represents one 4KB page (0=free, 1=allocated)
%%% - First-fit allocation with hint-based optimization
%%% - Contiguous allocation support for DMA/large buffers
%%% - Reserved region marking (kernel, UEFI structures, framebuffer)
%%%
%%% NO OTP dependencies - pure Erlang for bare metal.
%%%
-module(vbeam_page_alloc).
-export([
    init/1,
    alloc_page/1,
    alloc_pages/2,
    free_page/2,
    free_pages/2,
    mark_reserved/3,
    stats/1,
    alloc_contiguous/2
]).

-define(PAGE_SIZE, 4096).
-define(KERNEL_RESERVED_SIZE, 2 * 1024 * 1024). %% 2MB for kernel + boot structures

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

-type state() :: #{
    bitmap => binary(),
    page_size => pos_integer(),
    total_pages => non_neg_integer(),
    free_count => non_neg_integer(),
    next_hint => non_neg_integer(),
    reserved_end => non_neg_integer()  % Last page of reserved region
}.

-type phys_addr() :: non_neg_integer().

%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------

%% @doc Initialize allocator with total physical memory size
-spec init(non_neg_integer()) -> state().
init(TotalMemoryBytes) ->
    TotalPages = TotalMemoryBytes div ?PAGE_SIZE,
    BitmapSize = (TotalPages + 7) div 8, %% Round up to nearest byte
    Bitmap = <<0:(BitmapSize * 8)>>, %% All pages initially free

    %% Mark first 2MB as reserved (kernel + boot structures)
    ReservedPages = ?KERNEL_RESERVED_SIZE div ?PAGE_SIZE,
    State0 = #{
        bitmap => Bitmap,
        page_size => ?PAGE_SIZE,
        total_pages => TotalPages,
        free_count => TotalPages,
        next_hint => 0,
        reserved_end => ReservedPages - 1
    },

    %% Reserve the first 2MB
    mark_reserved_pages(State0, 0, ReservedPages - 1).

%% @doc Allocate a single page
-spec alloc_page(state()) -> {ok, phys_addr(), state()} | {error, out_of_memory}.
alloc_page(State) ->
    case alloc_contiguous(State, 1) of
        {ok, Addr, NewState} -> {ok, Addr, NewState};
        Error -> Error
    end.

%% @doc Allocate multiple pages (not necessarily contiguous)
-spec alloc_pages(state(), pos_integer()) -> {ok, [phys_addr()], state()} | {error, term()}.
alloc_pages(State, Count) ->
    alloc_pages_loop(State, Count, []).

%% @doc Free a single page
-spec free_page(state(), phys_addr()) -> state().
free_page(State, PhysAddr) ->
    PageNum = PhysAddr div ?PAGE_SIZE,
    #{bitmap := Bitmap, free_count := FreeCount, reserved_end := ReservedEnd} = State,

    %% Check if page is in reserved region
    case PageNum =< ReservedEnd of
        true ->
            %% Page is reserved, don't free it
            State;
        false ->
            %% Check if page is actually allocated before freeing
            case get_bit(Bitmap, PageNum) of
                1 ->
                    %% Page is allocated - free it
                    NewBitmap = clear_bit(Bitmap, PageNum),
                    State#{
                        bitmap := NewBitmap,
                        free_count := FreeCount + 1
                    };
                0 ->
                    %% Page is already free - double-free, return unchanged
                    State
            end
    end.

%% @doc Free multiple pages
-spec free_pages(state(), [phys_addr()]) -> state().
free_pages(State, PhysAddrs) ->
    lists:foldl(fun(Addr, S) -> free_page(S, Addr) end, State, PhysAddrs).

%% @doc Mark a physical address range as reserved
-spec mark_reserved(state(), phys_addr(), phys_addr()) -> state().
mark_reserved(State, StartAddr, EndAddr) when StartAddr > EndAddr ->
    %% Invalid range - return state unchanged
    State;
mark_reserved(State, StartAddr, EndAddr) ->
    StartPage = StartAddr div ?PAGE_SIZE,
    EndPage = EndAddr div ?PAGE_SIZE,
    mark_reserved_pages(State, StartPage, EndPage).

%% @doc Get allocator statistics
-spec stats(state()) -> #{atom() => non_neg_integer()}.
stats(#{total_pages := Total, free_count := Free}) ->
    #{
        total => Total,
        free => Free,
        allocated => Total - Free
    }.

%% @doc Allocate N contiguous pages
-spec alloc_contiguous(state(), pos_integer()) -> {ok, phys_addr(), state()} | {error, term()}.
alloc_contiguous(#{free_count := Free}, Count) when Free < Count ->
    {error, out_of_memory};
alloc_contiguous(State, Count) ->
    #{bitmap := Bitmap,
      total_pages := Total,
      next_hint := Hint,
      free_count := FreeCount} = State,

    case find_contiguous(Bitmap, Count, Hint, Total) of
        {ok, StartPage} ->
            %% Mark pages as allocated
            NewBitmap = mark_pages_allocated(Bitmap, StartPage, Count),
            NewState = State#{
                bitmap := NewBitmap,
                free_count := FreeCount - Count,
                next_hint := (StartPage + Count) rem Total
            },
            PhysAddr = StartPage * ?PAGE_SIZE,
            {ok, PhysAddr, NewState};
        not_found ->
            {error, no_contiguous_block}
    end.

%%% ----------------------------------------------------------------------------
%%% Internal Helpers
%%% ----------------------------------------------------------------------------

%% @doc Mark a range of pages as reserved
mark_reserved_pages(State, StartPage, EndPage) ->
    #{bitmap := Bitmap, free_count := FreeCount, total_pages := Total} = State,

    %% Clamp EndPage to valid range
    ClampedEnd = min(EndPage, Total - 1),

    %% If StartPage is beyond total, nothing to do
    case StartPage >= Total of
        true -> State;
        false ->
            %% Count how many were actually free before marking
            FreeInRange = count_free_in_range(Bitmap, StartPage, ClampedEnd),

            NumPages = ClampedEnd - StartPage + 1,
            NewBitmap = mark_pages_allocated(Bitmap, StartPage, NumPages),
            State#{
                bitmap := NewBitmap,
                free_count := FreeCount - FreeInRange
            }
    end.

%% @doc Count free pages in a range
count_free_in_range(Bitmap, StartPage, EndPage) ->
    count_free_in_range_loop(Bitmap, StartPage, EndPage, 0).

count_free_in_range_loop(_Bitmap, Page, EndPage, Count) when Page > EndPage ->
    Count;
count_free_in_range_loop(Bitmap, Page, EndPage, Count) ->
    NewCount = case get_bit(Bitmap, Page) of
        0 -> Count + 1;
        1 -> Count
    end,
    count_free_in_range_loop(Bitmap, Page + 1, EndPage, NewCount).

%% @doc Mark N consecutive pages as allocated starting from StartPage
mark_pages_allocated(Bitmap, StartPage, Count) ->
    mark_pages_loop(Bitmap, StartPage, Count).

mark_pages_loop(Bitmap, _Page, Count) when Count =< 0 ->
    Bitmap;
mark_pages_loop(Bitmap, Page, Count) ->
    NewBitmap = set_bit(Bitmap, Page),
    mark_pages_loop(NewBitmap, Page + 1, Count - 1).

%% @doc Find N contiguous free pages starting from Hint
find_contiguous(Bitmap, Count, Hint, Total) ->
    %% Try from hint to end
    case find_contiguous_range(Bitmap, Count, Hint, Total) of
        {ok, _} = Result -> Result;
        not_found ->
            %% Wrap around: try from 0 to hint
            find_contiguous_range(Bitmap, Count, 0, Hint)
    end.

find_contiguous_range(_Bitmap, Count, Start, End) when Start + Count > End ->
    not_found;
find_contiguous_range(Bitmap, Count, Start, End) ->
    case check_contiguous(Bitmap, Start, Count) of
        true -> {ok, Start};
        false -> find_contiguous_range(Bitmap, Count, Start + 1, End)
    end.

%% @doc Check if N consecutive pages are free starting from StartPage
check_contiguous(_Bitmap, _Start, 0) ->
    true;
check_contiguous(Bitmap, Start, Count) ->
    case get_bit(Bitmap, Start) of
        0 -> check_contiguous(Bitmap, Start + 1, Count - 1);
        1 -> false
    end.

%% @doc Allocate multiple non-contiguous pages
alloc_pages_loop(State, 0, Acc) ->
    {ok, lists:reverse(Acc), State};
alloc_pages_loop(State, Count, Acc) ->
    case alloc_page(State) of
        {ok, Addr, NewState} ->
            alloc_pages_loop(NewState, Count - 1, [Addr | Acc]);
        {error, _} = Error ->
            %% Free already allocated pages and return error
            FinalState = free_pages(State, Acc),
            {Error, FinalState}
    end.

%%% ----------------------------------------------------------------------------
%%% Bit Manipulation
%%% ----------------------------------------------------------------------------

%% @doc Get bit at position N in bitmap (0 or 1)
get_bit(Bitmap, N) when N >= 0 ->
    BitmapSize = byte_size(Bitmap) * 8,
    case N >= BitmapSize of
        true ->
            %% Out of range, treat as free (0)
            0;
        false ->
            ByteIndex = N div 8,
            BitIndex = N rem 8,
            <<_:ByteIndex/binary, Byte:8, _/binary>> = Bitmap,
            (Byte bsr (7 - BitIndex)) band 1
    end;
get_bit(_Bitmap, _N) ->
    %% Negative index, treat as free
    0.

%% @doc Set bit at position N to 1
set_bit(Bitmap, N) when N >= 0 ->
    BitmapSize = byte_size(Bitmap) * 8,
    case N >= BitmapSize of
        true ->
            %% Out of range, return unchanged
            Bitmap;
        false ->
            ByteIndex = N div 8,
            BitIndex = N rem 8,
            <<Before:ByteIndex/binary, Byte:8, After/binary>> = Bitmap,
            Mask = 1 bsl (7 - BitIndex),
            NewByte = Byte bor Mask,
            <<Before/binary, NewByte:8, After/binary>>
    end;
set_bit(Bitmap, _N) ->
    %% Negative index, return unchanged
    Bitmap.

%% @doc Clear bit at position N to 0
clear_bit(Bitmap, N) when N >= 0 ->
    BitmapSize = byte_size(Bitmap) * 8,
    case N >= BitmapSize of
        true ->
            %% Out of range, return unchanged
            Bitmap;
        false ->
            ByteIndex = N div 8,
            BitIndex = N rem 8,
            <<Before:ByteIndex/binary, Byte:8, After/binary>> = Bitmap,
            Mask = bnot (1 bsl (7 - BitIndex)),
            NewByte = Byte band Mask,
            <<Before/binary, NewByte:8, After/binary>>
    end;
clear_bit(Bitmap, _N) ->
    %% Negative index, return unchanged
    Bitmap.
