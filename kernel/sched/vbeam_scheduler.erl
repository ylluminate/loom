%%% @doc BEAM Process Scheduler for Loom OS.
%%%
%%% Round-robin scheduler that runs on timer interrupt ticks.
%%% Receives {irq, 32, Timestamp} messages from vbeam_irq_bridge when timer fires.
%%%
%%% Architecture:
%%%   Timer IRQ → vbeam_irq_bridge → {irq, 32, Ts} → vbeam_scheduler → schedule_next
%%%
%%% Each BEAM "process" in our OS is a lightweight task with its own:
%%%   - Heap (via vbeam_heap)
%%%   - Mailbox (for message passing)
%%%   - Status (ready/running/blocked/dead)
%%%   - Priority level (affects scheduling frequency)
%%%
%%% The scheduler maintains separate queues per priority level and implements
%%% weighted round-robin: high priority runs 2x as often, low runs 0.5x.
%%%
%%% NO actual Erlang process spawning — these are BEAM processes in the OS sense,
%%% implemented as gen_server state. On bare metal, this would be the only scheduler.
%%%
%%% @end
-module(vbeam_scheduler).

-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    spawn_process/2,
    kill_process/1,
    get_process/1,
    list_processes/0,
    send_message/2,
    receive_message/1,
    stats/0,
    pit_init_code/1,
    pic_init_code/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(IDLE_PID, 0).
-define(IRQ_TIMER, 32).

%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type pid_internal() :: non_neg_integer().
-type priority() :: high | normal | low.
-type process_status() :: ready | running | blocked | dead.

-type process_state() :: #{
    pid => pid_internal(),
    module => atom(),
    function => atom(),
    status => process_status(),
    heap => term(),              % vbeam_heap state
    reductions => non_neg_integer(),
    priority => priority(),
    mailbox => queue:queue(),
    created_at => integer(),
    last_scheduled => integer()
}.

-record(state, {
    config :: #{atom() => term()},
    processes :: #{pid_internal() => process_state()},
    ready_high :: queue:queue(),     % High priority ready queue
    ready_normal :: queue:queue(),   % Normal priority ready queue
    ready_low :: queue:queue(),      % Low priority ready queue
    current_pid :: pid_internal() | undefined,
    next_pid :: pid_internal(),
    page_alloc :: term(),            % vbeam_page_alloc state
    heap_registry :: term(),         % vbeam_heap registry
    ticks :: non_neg_integer(),
    context_switches :: non_neg_integer(),
    idle_ticks :: non_neg_integer(),
    start_time :: integer(),
    quota_state :: #{atom() => non_neg_integer()}  % Track quota per priority
}).

%%% ==========================================================================
%%% Public API
%%% ==========================================================================

%% @doc Start the scheduler as a gen_server.
%% Config: #{time_slice => Millis, max_processes => N}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Spawn a new BEAM process with Module:Function/0 entry point.
%% Returns internal PID (not Erlang pid).
-spec spawn_process(atom(), atom()) -> {ok, pid_internal()} | {error, term()}.
spawn_process(Module, Function) ->
    gen_server:call(?MODULE, {spawn_process, Module, Function}).

%% @doc Kill a process by internal PID, freeing its heap.
-spec kill_process(pid_internal()) -> ok | {error, not_found}.
kill_process(Pid) ->
    gen_server:call(?MODULE, {kill_process, Pid}).

%% @doc Get process info by internal PID.
-spec get_process(pid_internal()) -> {ok, process_state()} | {error, not_found}.
get_process(Pid) ->
    gen_server:call(?MODULE, {get_process, Pid}).

%% @doc List all processes with their status.
-spec list_processes() -> [process_state()].
list_processes() ->
    gen_server:call(?MODULE, list_processes).

%% @doc Send a message to a process's mailbox.
%% If process is blocked, it becomes ready.
-spec send_message(pid_internal(), term()) -> ok | {error, not_found}.
send_message(ToPid, Message) ->
    gen_server:call(?MODULE, {send_message, ToPid, Message}).

%% @doc Receive next message from process's mailbox.
%% Returns {ok, Message}, {error, empty}, or {error, not_found}.
%% If empty, process becomes blocked.
-spec receive_message(pid_internal()) -> {ok, term()} | {error, empty | not_found}.
receive_message(Pid) ->
    gen_server:call(?MODULE, {receive_message, Pid}).

%% @doc Get scheduler statistics.
-spec stats() -> #{atom() => term()}.
stats() ->
    gen_server:call(?MODULE, stats).

%% @doc Generate x86_64 machine code to initialize the PIT (Programmable Interval Timer).
%%
%% The PIT operates at 1.193182 MHz. To get desired frequency:
%%   divisor = 1193182 / desired_hz
%%
%% For 1000 Hz (1ms tick): divisor = 1193
%%
%% Steps:
%%   1. Send command byte to 0x43 (channel 0, mode 2, binary, 16-bit)
%%   2. Send divisor low byte to 0x40
%%   3. Send divisor high byte to 0x40
-spec pit_init_code(pos_integer()) -> binary().
pit_init_code(FreqHz) when FreqHz > 0 ->
    Divisor = 1193182 div FreqHz,

    %% Validate divisor range
    case Divisor >= 1 andalso Divisor =< 65535 of
        false ->
            erlang:error({invalid_frequency, FreqHz, Divisor});
        true ->
            ok
    end,

    LowByte = Divisor band 16#FF,
    HighByte = (Divisor bsr 8) band 16#FF,

    iolist_to_binary([
        %% Command: Channel 0, Mode 2 (rate generator), Binary, 16-bit
        %% Command byte: 00110100 = 0x34
        %%   Bits 7-6: Channel select (00 = channel 0)
        %%   Bits 5-4: Access mode (11 = lobyte/hibyte)
        %%   Bits 3-1: Operating mode (010 = mode 2)
        %%   Bit 0:    BCD/Binary (0 = binary)
        <<16#B0, 16#34>>,                           % mov al, 0x34
        <<16#E6, 16#43>>,                           % out 0x43, al

        %% Send divisor low byte to channel 0 data port
        <<16#B0, LowByte:8>>,                       % mov al, LowByte
        <<16#E6, 16#40>>,                           % out 0x40, al

        %% Send divisor high byte to channel 0 data port
        <<16#B0, HighByte:8>>,                      % mov al, HighByte
        <<16#E6, 16#40>>                            % out 0x40, al
    ]).

%% @doc Generate x86_64 machine code to initialize the 8259 PIC.
%%
%% The PIC (Programmable Interrupt Controller) needs to be remapped because
%% the default IRQ0-7 (vectors 8-15) conflict with CPU exceptions.
%%
%% We remap:
%%   Master PIC: IRQ0-7 → vectors 32-39
%%   Slave PIC:  IRQ8-15 → vectors 40-47
%%
%% Initialization sequence (ICW1-ICW4):
%%   ICW1: 0x11 (cascade mode, ICW4 needed)
%%   ICW2: Vector offset (32 for master, 40 for slave)
%%   ICW3: Cascade identity (master: bit 2 set, slave: cascade on IRQ2)
%%   ICW4: 0x01 (8086 mode)
%%
%% Then unmask only IRQ0 (timer) by setting all other bits in IMR.
-spec pic_init_code() -> binary().
pic_init_code() ->
    iolist_to_binary([
        %% ICW1: Initialize both PICs (cascade mode, ICW4 needed)
        <<16#B0, 16#11>>,                           % mov al, 0x11
        <<16#E6, 16#20>>,                           % out 0x20, al (master command)
        <<16#E6, 16#A0>>,                           % out 0xA0, al (slave command)

        %% ICW2: Vector offset (master IRQ0-7 → vectors 32-39)
        <<16#B0, 16#20>>,                           % mov al, 32
        <<16#E6, 16#21>>,                           % out 0x21, al (master data)

        %% ICW2: Vector offset (slave IRQ8-15 → vectors 40-47)
        <<16#B0, 16#28>>,                           % mov al, 40
        <<16#E6, 16#A1>>,                           % out 0xA1, al (slave data)

        %% ICW3: Master PIC has slave on IRQ2 (bit 2 = 1 << 2 = 4)
        <<16#B0, 16#04>>,                           % mov al, 4
        <<16#E6, 16#21>>,                           % out 0x21, al (master data)

        %% ICW3: Slave PIC cascade identity (IRQ2 on master)
        <<16#B0, 16#02>>,                           % mov al, 2
        <<16#E6, 16#A1>>,                           % out 0xA1, al (slave data)

        %% ICW4: 8086 mode for both PICs
        <<16#B0, 16#01>>,                           % mov al, 0x01
        <<16#E6, 16#21>>,                           % out 0x21, al (master data)
        <<16#E6, 16#A1>>,                           % out 0xA1, al (slave data)

        %% Unmask only IRQ0 (timer): IMR = 11111110 = 0xFE
        <<16#B0, 16#FE>>,                           % mov al, 0xFE
        <<16#E6, 16#21>>,                           % out 0x21, al (master IMR)

        %% Mask all slave IRQs
        <<16#B0, 16#FF>>,                           % mov al, 0xFF
        <<16#E6, 16#A1>>                            % out 0xA1, al (slave IMR)
    ]).

%%% ==========================================================================
%%% gen_server Callbacks
%%% ==========================================================================

init(Config) ->
    %% Initialize page allocator (1GB total, 4KB pages)
    TotalMemory = 1024 * 1024 * 1024,
    PageAllocState = vbeam_page_alloc:init(TotalMemory),

    %% Initialize heap registry
    HeapRegistry = vbeam_heap:registry_new(),

    %% Create idle process (PID 0)
    {ok, IdleHeap, PageAllocState1} = vbeam_heap:new_heap(PageAllocState, small),
    IdleProcess = #{
        pid => ?IDLE_PID,
        module => idle,
        function => loop,
        status => ready,
        heap => IdleHeap,
        reductions => 0,
        priority => low,
        mailbox => queue:new(),
        created_at => erlang:monotonic_time(nanosecond),
        last_scheduled => 0
    },

    HeapRegistry1 = vbeam_heap:registry_add(HeapRegistry, ?IDLE_PID, IdleHeap),

    State = #state{
        config = Config,
        processes = #{?IDLE_PID => IdleProcess},
        ready_high = queue:new(),
        ready_normal = queue:new(),
        ready_low = queue:in(?IDLE_PID, queue:new()),  % Idle starts in low queue
        current_pid = undefined,
        next_pid = 1,
        page_alloc = PageAllocState1,
        heap_registry = HeapRegistry1,
        ticks = 0,
        context_switches = 0,
        idle_ticks = 0,
        start_time = erlang:monotonic_time(millisecond),
        quota_state = #{high => 0, normal => 0, low => 0}
    },

    {ok, State}.

handle_call({spawn_process, Module, Function}, _From, State) ->
    #state{processes = Processes, next_pid = NextPid, page_alloc = PageAlloc,
           heap_registry = HeapReg, ready_normal = ReadyNormal, config = Config} = State,

    %% Check max_processes limit
    MaxProcesses = maps:get(max_processes, Config, infinity),
    case MaxProcesses =/= infinity andalso map_size(Processes) >= MaxProcesses of
        true ->
            {reply, {error, max_processes_reached}, State};
        false ->
            %% Allocate heap for new process
            case vbeam_heap:new_heap(PageAlloc, medium) of
                {ok, Heap, NewPageAlloc} ->
                    NewProcess = #{
                        pid => NextPid,
                        module => Module,
                        function => Function,
                        status => ready,
                        heap => Heap,
                        reductions => 0,
                        priority => normal,
                        mailbox => queue:new(),
                        created_at => erlang:monotonic_time(nanosecond),
                        last_scheduled => 0
                    },

                    NewProcesses = Processes#{NextPid => NewProcess},
                    NewHeapReg = vbeam_heap:registry_add(HeapReg, NextPid, Heap),
                    NewReadyNormal = queue:in(NextPid, ReadyNormal),

                    NewState = State#state{
                        processes = NewProcesses,
                        next_pid = NextPid + 1,
                        page_alloc = NewPageAlloc,
                        heap_registry = NewHeapReg,
                        ready_normal = NewReadyNormal
                    },

                    {reply, {ok, NextPid}, NewState};

                {error, Reason, FailPageAlloc} ->
                    %% Error already wrapped - don't double-wrap
                    {reply, {error, Reason}, State#state{page_alloc = FailPageAlloc}};
                {Error, FailPageAlloc} ->
                    %% Legacy pattern or unwrapped error
                    {reply, {error, Error}, State#state{page_alloc = FailPageAlloc}}
            end
    end;

handle_call({kill_process, Pid}, _From, State) ->
    #state{processes = Processes, page_alloc = PageAlloc, heap_registry = HeapReg} = State,

    case maps:get(Pid, Processes, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #{heap := Heap} ->
            %% Free heap
            NewPageAlloc = vbeam_heap:free_heap(Heap, PageAlloc),
            NewHeapReg = vbeam_heap:registry_remove(HeapReg, Pid),
            NewProcesses = maps:remove(Pid, Processes),

            %% Remove from all queues
            NewState = remove_from_queues(Pid, State#state{
                processes = NewProcesses,
                page_alloc = NewPageAlloc,
                heap_registry = NewHeapReg
            }),

            {reply, ok, NewState}
    end;

handle_call({get_process, Pid}, _From, #state{processes = Processes} = State) ->
    case maps:get(Pid, Processes, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Process -> {reply, {ok, Process}, State}
    end;

handle_call(list_processes, _From, #state{processes = Processes} = State) ->
    ProcessList = maps:values(Processes),
    {reply, ProcessList, State};

handle_call({send_message, ToPid, Message}, _From, State) ->
    #state{processes = Processes} = State,

    case maps:get(ToPid, Processes, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #{mailbox := Mailbox, status := Status} = Process ->
            %% NOTE: Messages are shared-heap (not copied). This matches our modeled semantics.
            %% On bare metal with strict per-process heaps, this would require deep-copy.
            NewMailbox = queue:in(Message, Mailbox),
            UpdatedProcess = Process#{mailbox => NewMailbox},
            NewProcesses = Processes#{ToPid => UpdatedProcess},

            %% If process was blocked, move to ready
            NewState = case Status of
                blocked ->
                    make_ready(ToPid, UpdatedProcess, State#state{processes = NewProcesses});
                _ ->
                    State#state{processes = NewProcesses}
            end,

            {reply, ok, NewState}
    end;

handle_call({receive_message, Pid}, _From, #state{processes = Processes} = State) ->
    case maps:get(Pid, Processes, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #{mailbox := Mailbox} = Process ->
            %% NOTE: Returned message is shared-heap (not copied). This matches our modeled semantics.
            case queue:out(Mailbox) of
                {{value, Message}, NewMailbox} ->
                    UpdatedProcess = Process#{mailbox => NewMailbox},
                    NewProcesses = Processes#{Pid => UpdatedProcess},
                    {reply, {ok, Message}, State#state{processes = NewProcesses}};
                {empty, _} ->
                    %% Transition to blocked state and remove from ready queues
                    UpdatedProcess = Process#{status => blocked},
                    NewProcesses = Processes#{Pid => UpdatedProcess},
                    NewState = remove_from_queues(Pid, State#state{processes = NewProcesses}),
                    {reply, {error, empty}, NewState}
            end
    end;

handle_call(stats, _From, State) ->
    #state{ticks = Ticks, context_switches = CS, idle_ticks = IdleTicks,
           processes = Processes, start_time = StartTime} = State,

    StatusCounts = count_by_status(maps:values(Processes)),
    Uptime = erlang:monotonic_time(millisecond) - StartTime,

    Stats = #{
        total_ticks => Ticks,
        context_switches => CS,
        processes => #{
            total => maps:size(Processes),
            ready => maps:get(ready, StatusCounts, 0),
            running => maps:get(running, StatusCounts, 0),
            blocked => maps:get(blocked, StatusCounts, 0),
            dead => maps:get(dead, StatusCounts, 0)
        },
        idle_ticks => IdleTicks,
        uptime_ms => Uptime
    },

    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle timer interrupt from IRQ bridge
handle_info({irq, ?IRQ_TIMER, _Timestamp}, State) ->
    NewState = tick(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{processes = Processes, page_alloc = PageAlloc} = _State) ->
    %% Free all process heaps
    _FinalPageAlloc = maps:fold(fun(_Pid, #{heap := Heap}, AccPageAlloc) ->
        vbeam_heap:free_heap(Heap, AccPageAlloc)
    end, PageAlloc, Processes),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ==========================================================================
%%% Internal Scheduling Logic
%%% ==========================================================================

%% @doc Process a timer tick: preempt current, schedule next
tick(#state{ticks = Ticks, current_pid = CurrentPid} = State) ->
    State1 = State#state{ticks = Ticks + 1},

    %% Preempt current process if running
    State2 = case CurrentPid of
        undefined -> State1;
        Pid -> preempt(Pid, State1)
    end,

    %% Schedule next process
    schedule_next(State2).

%% @doc Preempt current process, move back to ready queue
preempt(Pid, #state{processes = Processes, config = Config} = State) ->
    case maps:get(Pid, Processes, undefined) of
        undefined ->
            State;
        #{status := running, priority := Priority} = Process ->
            TimeSlice = maps:get(time_slice, Config, 1000),
            UpdatedProcess = Process#{
                status => ready,
                reductions => maps:get(reductions, Process, 0) + TimeSlice
            },
            NewProcesses = Processes#{Pid => UpdatedProcess},

            %% Add back to appropriate ready queue
            State1 = State#state{processes = NewProcesses, current_pid = undefined},
            add_to_ready_queue(Pid, Priority, State1);
        _ ->
            %% Not running (blocked or dead), don't preempt
            State
    end.

%% @doc Schedule next process from ready queues with quota rotation
%% Quota: high:2 per tick, normal:1 per tick, low:1 every 2 ticks
%% Only applies when multiple priority levels have runnable processes
schedule_next(#state{ready_high = High, ready_normal = Normal, ready_low = Low,
                     ticks = _Ticks, processes = _Processes,
                     context_switches = _CS, idle_ticks = _IdleTicks,
                     quota_state = _Quota} = State) ->
    %% Check which queues have runnable processes
    HasHigh = not queue:is_empty(High),
    HasNormal = not queue:is_empty(Normal),
    HasLow = not queue:is_empty(Low),

    %% Determine if quota rotation applies (multiple priority levels active)
    UseQuota = (HasHigh andalso HasNormal) orelse
               (HasHigh andalso HasLow) orelse
               (HasNormal andalso HasLow),

    case UseQuota of
        false ->
            %% Simple priority: only one level has processes, use strict priority
            schedule_next_strict(State);
        true ->
            %% Quota rotation: prevent starvation
            schedule_next_quota(State)
    end.

%% Strict priority when only one level is active
schedule_next_strict(#state{ready_high = High, ready_normal = Normal, ready_low = Low,
                            ticks = Ticks, context_switches = CS, idle_ticks = IdleTicks} = State) ->
    TickMod = Ticks rem 2,
    case queue:out(High) of
        {{value, Pid}, NewHigh} ->
            run_process(Pid, State#state{ready_high = NewHigh, context_switches = CS + 1});
        {empty, _} ->
            case queue:out(Normal) of
                {{value, Pid}, NewNormal} ->
                    run_process(Pid, State#state{ready_normal = NewNormal, context_switches = CS + 1});
                {empty, _} ->
                    case TickMod =:= 0 andalso queue:out(Low) of
                        {{value, Pid}, NewLow} ->
                            IsIdle = Pid =:= ?IDLE_PID,
                            NewIdleTicks = case IsIdle of true -> IdleTicks + 1; false -> IdleTicks end,
                            run_process(Pid, State#state{ready_low = NewLow, context_switches = CS + 1,
                                                         idle_ticks = NewIdleTicks});
                        _ ->
                            State#state{idle_ticks = IdleTicks + 1}
                    end
            end
    end.

%% Quota-based scheduling to prevent starvation
schedule_next_quota(#state{ready_high = High, ready_normal = Normal, ready_low = Low,
                           ticks = Ticks, context_switches = CS, idle_ticks = IdleTicks,
                           quota_state = Quota} = State) ->
    HighQuota = maps:get(high, Quota, 0),
    NormalQuota = maps:get(normal, Quota, 0),
    LowQuota = maps:get(low, Quota, 0),
    TickMod = Ticks rem 2,

    %% High gets 2 per tick
    case HighQuota < 2 andalso queue:out(High) of
        {{value, Pid}, NewHigh} ->
            NewQuota = Quota#{high := HighQuota + 1},
            run_process(Pid, State#state{ready_high = NewHigh, context_switches = CS + 1,
                                         quota_state = NewQuota});
        _ ->
            %% Normal gets 1 per tick
            case NormalQuota < 1 andalso queue:out(Normal) of
                {{value, Pid}, NewNormal} ->
                    NewQuota = Quota#{normal := NormalQuota + 1},
                    run_process(Pid, State#state{ready_normal = NewNormal, context_switches = CS + 1,
                                                 quota_state = NewQuota});
                _ ->
                    %% Low gets 1 every 2 ticks
                    case TickMod =:= 0 andalso LowQuota < 1 andalso queue:out(Low) of
                        {{value, Pid}, NewLow} ->
                            NewQuota = Quota#{low := LowQuota + 1},
                            IsIdle = Pid =:= ?IDLE_PID,
                            NewIdleTicks = case IsIdle of true -> IdleTicks + 1; false -> IdleTicks end,
                            run_process(Pid, State#state{ready_low = NewLow, context_switches = CS + 1,
                                                         idle_ticks = NewIdleTicks, quota_state = NewQuota});
                        _ ->
                            %% Reset quotas and try again
                            NewQuota = #{high => 0, normal => 0, low => 0},
                            schedule_next_strict(State#state{quota_state = NewQuota})
                    end
            end
    end.

%% @doc Mark process as running and set as current
run_process(Pid, #state{processes = Processes} = State) ->
    case maps:get(Pid, Processes, undefined) of
        undefined ->
            %% Process disappeared, continue
            schedule_next(State);
        Process ->
            UpdatedProcess = Process#{
                status => running,
                last_scheduled => erlang:monotonic_time(nanosecond)
            },
            NewProcesses = Processes#{Pid => UpdatedProcess},
            State#state{processes = NewProcesses, current_pid = Pid}
    end.

%% @doc Add process to appropriate ready queue by priority
add_to_ready_queue(Pid, high, #state{ready_high = Queue} = State) ->
    State#state{ready_high = queue:in(Pid, Queue)};
add_to_ready_queue(Pid, normal, #state{ready_normal = Queue} = State) ->
    State#state{ready_normal = queue:in(Pid, Queue)};
add_to_ready_queue(Pid, low, #state{ready_low = Queue} = State) ->
    State#state{ready_low = queue:in(Pid, Queue)}.

%% @doc Make blocked process ready (when it receives a message)
make_ready(Pid, #{priority := Priority} = Process, #state{processes = Processes} = State) ->
    UpdatedProcess = Process#{status => ready},
    NewProcesses = Processes#{Pid => UpdatedProcess},
    add_to_ready_queue(Pid, Priority, State#state{processes = NewProcesses}).

%% @doc Remove process from all ready queues
remove_from_queues(Pid, #state{ready_high = High, ready_normal = Normal, ready_low = Low} = State) ->
    State#state{
        ready_high = queue_remove(Pid, High),
        ready_normal = queue_remove(Pid, Normal),
        ready_low = queue_remove(Pid, Low)
    }.

%% @doc Remove element from queue
queue_remove(Pid, Queue) ->
    List = queue:to_list(Queue),
    FilteredList = [P || P <- List, P =/= Pid],
    queue:from_list(FilteredList).

%% @doc Count processes by status
count_by_status(Processes) ->
    lists:foldl(fun(#{status := Status}, Acc) ->
        maps:update_with(Status, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Processes).
