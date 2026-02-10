%%% @doc Interrupt-to-Message Bridge for Loom OS BEAM Kernel.
%%%
%%% Hardware interrupts are converted to BEAM messages via a lock-free ring buffer.
%%% This is the core of the QNX-like architecture where every driver is a BEAM process
%%% that receives messages from hardware.
%%%
%%% Architecture:
%%%   IRQ fires → CPU runs ISR → ISR writes {irq_num, timestamp} to ring buffer
%%%                                        ↓
%%%   BEAM scheduler tick → polls ring buffer → delivers message to registered process
%%%                                        ↓
%%%   Driver gen_server receives {irq, N} → handles interrupt in Erlang
%%%
%%% Ring buffer is implemented as Erlang map for OTP environment (simulates fixed memory).
%%% On bare metal, this would be a fixed physical memory region.
%%%
%%% @end
-module(vbeam_irq_bridge).

-behaviour(gen_server).

%% Public API
-export([start_link/0,
         register_handler/2,
         unregister_handler/1,
         get_handlers/0,
         simulate_irq/1,
         tick/1,
         isr_ring_buffer_write/0]).

%% Ring buffer operations
-export([ring_new/1,
         ring_push/2,
         ring_pop/1,
         ring_pop_all/1,
         ring_size/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RING_BUFFER_ADDR, 16#100000).  % Physical address for bare metal
-define(DEFAULT_RING_SIZE, 256).       % Power of 2

%%% ==========================================================================
%%% Ring Buffer Implementation
%%% ==========================================================================

%% @doc Create a new ring buffer of size N (must be power of 2).
-spec ring_new(pos_integer()) -> map().
ring_new(Size) when Size > 0, Size band (Size - 1) =:= 0 ->
    #{head => 0,
      tail => 0,
      size => Size,
      data => #{}}.

%% @doc Push an event {IrqNum, Timestamp} to the ring buffer.
%% If buffer is full, oldest event is overwritten (circular behavior).
-spec ring_push({non_neg_integer(), non_neg_integer()}, map()) -> map().
ring_push({IrqNum, Timestamp}, #{head := Head, tail := Tail, size := Size, data := Data} = Buf) ->
    Index = Tail band (Size - 1),
    NewData = Data#{Index => {IrqNum, Timestamp}},
    NewTail = Tail + 1,

    %% Check if we're overwriting head (buffer full)
    %% Buffer is full when tail - head == size, so advancing tail will overflow
    NewHead = case NewTail - Head > Size of
        true -> Head + 1;  % Advance head (lose oldest)
        false -> Head
    end,

    Buf#{head => NewHead, tail => NewTail, data => NewData}.

%% @doc Pop the oldest event from the ring buffer.
%% Returns {ok, Event, NewBuf} or empty.
-spec ring_pop(map()) -> {ok, {non_neg_integer(), non_neg_integer()}, map()} | empty.
ring_pop(#{head := Head, tail := Tail, size := Size, data := Data} = Buf) when Head < Tail ->
    Index = Head band (Size - 1),
    Event = maps:get(Index, Data),
    NewData = maps:remove(Index, Data),
    NewBuf = Buf#{head => Head + 1, data => NewData},
    {ok, Event, NewBuf};
ring_pop(#{head := Head, tail := Tail}) when Head >= Tail ->
    empty.

%% @doc Pop all pending events from the ring buffer.
%% Returns list of events in order (oldest first).
-spec ring_pop_all(map()) -> {[{non_neg_integer(), non_neg_integer()}], map()}.
ring_pop_all(Buf) ->
    ring_pop_all(Buf, []).

ring_pop_all(Buf, Acc) ->
    case ring_pop(Buf) of
        {ok, Event, NewBuf} ->
            ring_pop_all(NewBuf, [Event | Acc]);
        empty ->
            {lists:reverse(Acc), Buf}
    end.

%% @doc Return number of pending events in the ring buffer.
-spec ring_size(map()) -> non_neg_integer().
ring_size(#{head := Head, tail := Tail}) ->
    Tail - Head.

%%% ==========================================================================
%%% gen_server State
%%% ==========================================================================

-record(state, {
    ring_buffer :: map(),
    handlers :: #{non_neg_integer() => pid()},
    monitors :: #{reference() => non_neg_integer()}  % monitor_ref => irq_num
}).

%%% ==========================================================================
%%% Public API
%%% ==========================================================================

%% @doc Start the IRQ bridge as a gen_server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a PID to handle IRQ N.
%% Only one handler per IRQ number.
-spec register_handler(non_neg_integer(), pid()) -> ok.
register_handler(IrqNum, Pid) when is_integer(IrqNum), IrqNum >= 0, is_pid(Pid) ->
    gen_server:call(?MODULE, {register_handler, IrqNum, Pid}).

%% @doc Unregister handler for IRQ N.
-spec unregister_handler(non_neg_integer()) -> ok.
unregister_handler(IrqNum) when is_integer(IrqNum), IrqNum >= 0 ->
    gen_server:call(?MODULE, {unregister_handler, IrqNum}).

%% @doc Return map of IrqNum → Pid.
-spec get_handlers() -> #{non_neg_integer() => pid()}.
get_handlers() ->
    gen_server:call(?MODULE, get_handlers).

%% @doc Simulate an interrupt by pushing to the ring buffer.
%% For testing only. Real interrupts would be handled by ISR.
-spec simulate_irq(non_neg_integer()) -> ok.
simulate_irq(IrqNum) when is_integer(IrqNum), IrqNum >= 0 ->
    Timestamp = erlang:system_time(nanosecond),
    gen_server:cast(?MODULE, {simulate_irq, IrqNum, Timestamp}).

%% @doc Called on scheduler tick, polls ring buffer and delivers pending IRQs.
%% Returns {ok, DeliveredCount}.
-spec tick(pid() | atom()) -> {ok, non_neg_integer()}.
tick(ServerRef) ->
    gen_server:call(ServerRef, tick).

%% @doc Generate x86_64 machine code for ISR stub that writes to ring buffer.
%%
%% On bare metal, this ISR would:
%%   1. Save registers (rax, rdx)
%%   2. Load ring buffer base address
%%   3. Read tail pointer
%%   4. Compute index: tail & (size - 1)
%%   5. Write {irq_num, timestamp} to data[index]
%%   6. Increment tail
%%   7. Send EOI to APIC
%%   8. Restore registers
%%   9. iretq
%%
%% For now, returns minimal stub that demonstrates the structure.
-spec isr_ring_buffer_write() -> binary().
isr_ring_buffer_write() ->
    %% Ring buffer layout in physical memory:
    %%   [head:8][tail:8][size:8][data: N * 16 bytes]
    %% Each entry: [irq_num:8][timestamp:8]

    RingBufferBase = ?RING_BUFFER_ADDR,

    iolist_to_binary([
        %% Save registers (all GPRs that may be clobbered)
        <<16#50>>,                                              % push rax
        <<16#51>>,                                              % push rcx
        <<16#52>>,                                              % push rdx
        <<16#41, 16#50>>,                                       % push r8
        <<16#41, 16#51>>,                                       % push r9
        <<16#41, 16#52>>,                                       % push r10
        <<16#41, 16#53>>,                                       % push r11

        %% Load ring buffer pointers
        <<16#48, 16#B8>>, <<RingBufferBase:64/little>>,        % mov rax, RingBufferBase
        <<16#49, 16#89, 16#C3>>,                               % mov r11, rax (save base, rdtsc will clobber rax)
        <<16#48, 16#8B, 16#50, 16#08>>,                        % mov rdx, [rax+8] (tail)
        <<16#48, 16#8B, 16#48, 16#10>>,                        % mov rcx, [rax+16] (size)

        %% Compute index: rdx & (rcx - 1)
        <<16#48, 16#FF, 16#C9>>,                               % dec rcx (size - 1)
        <<16#48, 16#21, 16#CA>>,                               % and rdx, rcx

        %% Compute entry offset: index * 16 + 24 (header size)
        <<16#48, 16#C1, 16#E2, 16#04>>,                        % shl rdx, 4 (multiply by 16)
        <<16#48, 16#83, 16#C2, 16#18>>,                        % add rdx, 24

        %% Write IRQ number (from stack at [rsp+56], after 7 pushed regs)
        <<16#48, 16#8B, 16#4C, 16#24, 16#38>>,                 % mov rcx, [rsp+56]
        <<16#4A, 16#89, 16#0C, 16#1A>>,                        % mov [r11+rdx], rcx (use r11 base)

        %% Save entry offset to R8 (rdx will be clobbered by rdtsc)
        <<16#49, 16#89, 16#D0>>,                               % mov r8, rdx

        %% Write timestamp (rdtsc)
        <<16#0F, 16#31>>,                                       % rdtsc (edx:eax)
        <<16#48, 16#C1, 16#E2, 16#20>>,                        % shl rdx, 32
        <<16#48, 16#09, 16#C2>>,                               % or rdx, rax
        <<16#4B, 16#89, 16#54, 16#03, 16#08>>,                 % mov [r11+r8+8], rdx (use r11 base)

        %% Increment tail
        <<16#49, 16#8B, 16#53, 16#08>>,                        % mov rdx, [r11+8]
        <<16#48, 16#FF, 16#C2>>,                               % inc rdx
        <<16#49, 16#89, 16#53, 16#08>>,                        % mov [r11+8], rdx (use r11 base)

        %% Send EOI to LAPIC (0xFEE000B0)
        <<16#48, 16#B8, 16#B0, 16#00, 16#E0, 16#FE, 16#00, 16#00, 16#00, 16#00>>,  % mov rax, 0xFEE000B0
        <<16#C7, 16#00, 16#00, 16#00, 16#00, 16#00>>,          % mov dword [rax], 0

        %% Restore registers (in reverse order)
        <<16#41, 16#5B>>,                                       % pop r11
        <<16#41, 16#5A>>,                                       % pop r10
        <<16#41, 16#59>>,                                       % pop r9
        <<16#41, 16#58>>,                                       % pop r8
        <<16#5A>>,                                              % pop rdx
        <<16#59>>,                                              % pop rcx
        <<16#58>>,                                              % pop rax

        %% Return from interrupt
        <<16#48, 16#CF>>                                        % iretq
    ]).

%%% ==========================================================================
%%% gen_server Callbacks
%%% ==========================================================================

init([]) ->
    RingBuffer = ring_new(?DEFAULT_RING_SIZE),
    {ok, #state{ring_buffer = RingBuffer, handlers = #{}, monitors = #{}}}.

handle_call({register_handler, IrqNum, Pid}, _From, #state{handlers = Handlers, monitors = Monitors} = State) ->
    %% Check if an existing handler is registered for this IRQ
    {CleanedMonitors, CleanedHandlers} = case maps:get(IrqNum, Handlers, undefined) of
        undefined ->
            {Monitors, Handlers};
        _OldPid ->
            %% Find and demonitor the old handler's monitor ref
            OldMonitorRef = maps:fold(fun(Ref, Irq, Acc) ->
                case Irq =:= IrqNum of
                    true -> Ref;
                    false -> Acc
                end
            end, undefined, Monitors),
            case OldMonitorRef of
                undefined ->
                    {Monitors, Handlers};
                Ref ->
                    demonitor(Ref, [flush]),
                    {maps:remove(Ref, Monitors), maps:remove(IrqNum, Handlers)}
            end
    end,

    %% Monitor the new handler PID
    MonitorRef = monitor(process, Pid),
    NewHandlers = CleanedHandlers#{IrqNum => Pid},
    NewMonitors = CleanedMonitors#{MonitorRef => IrqNum},
    {reply, ok, State#state{handlers = NewHandlers, monitors = NewMonitors}};

handle_call({unregister_handler, IrqNum}, _From, #state{handlers = Handlers, monitors = Monitors} = State) ->
    %% Demonitor if handler exists
    NewMonitors = case maps:get(IrqNum, Handlers, undefined) of
        undefined ->
            Monitors;
        _Pid ->
            %% Find and remove monitor reference
            MonitorRef = maps:fold(fun(Ref, Irq, Acc) ->
                case Irq =:= IrqNum of
                    true -> Ref;
                    false -> Acc
                end
            end, undefined, Monitors),
            case MonitorRef of
                undefined -> Monitors;
                Ref ->
                    demonitor(Ref, [flush]),
                    maps:remove(Ref, Monitors)
            end
    end,
    NewHandlers = maps:remove(IrqNum, Handlers),
    {reply, ok, State#state{handlers = NewHandlers, monitors = NewMonitors}};

handle_call(get_handlers, _From, #state{handlers = Handlers} = State) ->
    {reply, Handlers, State};

handle_call(tick, _From, #state{ring_buffer = RingBuf, handlers = Handlers} = State) ->
    {Events, NewRingBuf} = ring_pop_all(RingBuf),

    %% Deliver each event to its registered handler (only if handler is alive)
    DeliveredCount = lists:foldl(fun({IrqNum, Timestamp}, Count) ->
        case maps:get(IrqNum, Handlers, undefined) of
            undefined ->
                %% No handler registered, drop event
                Count;
            Pid when is_pid(Pid) ->
                %% Only count as delivered if process is alive
                case is_process_alive(Pid) of
                    true ->
                        Pid ! {irq, IrqNum, Timestamp},
                        Count + 1;
                    false ->
                        %% Handler died but not yet cleaned up by monitor
                        Count
                end
        end
    end, 0, Events),

    {reply, {ok, DeliveredCount}, State#state{ring_buffer = NewRingBuf}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({simulate_irq, IrqNum, Timestamp}, #state{ring_buffer = RingBuf} = State) ->
    NewRingBuf = ring_push({IrqNum, Timestamp}, RingBuf),
    {noreply, State#state{ring_buffer = NewRingBuf}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle monitor DOWN messages when a handler dies
handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, #state{handlers = Handlers, monitors = Monitors} = State) ->
    case maps:get(MonitorRef, Monitors, undefined) of
        undefined ->
            %% Unknown monitor, ignore
            {noreply, State};
        IrqNum ->
            %% Remove dead handler
            NewHandlers = maps:remove(IrqNum, Handlers),
            NewMonitors = maps:remove(MonitorRef, Monitors),
            {noreply, State#state{handlers = NewHandlers, monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
