-module(vbeam_linuxkpi).
-export([
    %% Memory Management
    kmalloc/2, kfree/1, vmalloc/1, vfree/1, kzalloc/2,
    dma_alloc_coherent/4, dma_free_coherent/3, dma_free_coherent/4,

    %% Logging
    printk/2, dev_err/2, dev_warn/2, dev_info/2, dev_dbg/2,

    %% Synchronization
    spin_lock/1, spin_unlock/1, spin_lock_init/1,
    spin_lock_irqsave/2, spin_unlock_irqrestore/2,
    mutex_init/1, mutex_lock/1, mutex_unlock/1,
    init_waitqueue_head/1, wake_up/1, wake_up_interruptible/1,

    %% PCI
    pci_register_driver/1, pci_unregister_driver/1,
    pci_enable_device/1, pci_disable_device/1, pci_set_master/1,
    pci_resource_start/2, pci_resource_len/2,
    pci_iomap/3, pci_iounmap/2,

    %% Interrupts
    request_irq/5, request_threaded_irq/6, free_irq/2, enable_irq/1, disable_irq/1,

    %% Workqueues/Timers
    schedule_work/1, queue_work/2, mod_timer/2, del_timer/1, timer_fired/1, timer_fired/2,

    %% DMA
    dma_map_single/4, dma_unmap_single/4, dma_set_mask/2,

    %% Network Subsystem
    alloc_etherdev/1, register_netdev/1, unregister_netdev/1,
    netif_start_queue/1, netif_stop_queue/1, netif_wake_queue/1,
    netif_rx/1, napi_schedule/1, napi_complete/1,

    %% Internal initialization
    init/0
]).

%% Timer tracking table
-define(TIMER_TABLE, vbeam_linuxkpi_timers).

%% ==================================================================
%% Initialization
%% ==================================================================

%% @doc Initialize LinuxKPI subsystem (ETS tables, etc.)
%% BUG 11 FIX: Make init idempotent with try/catch
init() ->
    try
        case ets:whereis(?TIMER_TABLE) of
            undefined ->
                ets:new(?TIMER_TABLE, [named_table, public, set]),
                ok;
            _ ->
                ok
        end
    catch
        error:badarg ->
            %% Table already exists (race condition)
            ok
    end.

%% ==================================================================
%% Memory Management
%% ==================================================================

%% @doc Allocate kernel memory
%% Real implementation: allocate from BEAM heap, track in ETS for debugging
kmalloc(Size, _Flags) when is_integer(Size), Size > 0, Size =< 1073741824 ->
    %% BUG 6 FIX: Validate size to prevent OOM (max 1GB)
    log_kapi_call(kmalloc, [Size, _Flags]),
    %% Allocate BEAM binary (automatically managed)
    Ptr = make_ref(),
    try
        Memory = <<0:(Size*8)>>,
        %% TODO: Store in ETS table for tracking: {Ptr, Memory, Size}
        {ok, Ptr, Memory}
    catch
        error:system_limit -> {error, enomem};
        error:badarg -> {error, enomem}
    end;
kmalloc(Size, _Flags) ->
    log_kapi_call(kmalloc, [Size, _Flags]),
    %% Invalid size
    {error, einval}.

%% @doc Free kernel memory
kfree(Ptr) ->
    log_kapi_call(kfree, [Ptr]),
    %% Real implementation: remove from tracking ETS, BEAM GC handles actual free
    ok.

%% @doc Allocate virtually contiguous memory
vmalloc(Size) ->
    log_kapi_call(vmalloc, [Size]),
    %% Same as kmalloc for BEAM (no MMU distinction)
    kmalloc(Size, 0).

%% @doc Free vmalloc memory
vfree(Ptr) ->
    log_kapi_call(vfree, [Ptr]),
    kfree(Ptr).

%% @doc Allocate and zero memory
kzalloc(Size, Flags) ->
    log_kapi_call(kzalloc, [Size, Flags]),
    %% Already zeroed by default in BEAM binaries
    kmalloc(Size, Flags).

%% @doc Allocate DMA-coherent memory
%% Real implementation: allocate pinned memory, get physical address
dma_alloc_coherent(_Dev, Size, _DmaHandle, _Flags) ->
    log_kapi_call(dma_alloc_coherent, [_Dev, Size, _DmaHandle, _Flags]),
    %% TODO: Coordinate with vbeam_dma to get pinned physical memory
    case kmalloc(Size, 0) of
        {ok, Ptr, _Memory} ->
            DmaAddr = 16#F000_0000 + erlang:phash2(Ptr, 16#0FFF_FFFF),
            {ok, Ptr, DmaAddr};
        {error, _} = Error ->
            Error
    end.

%% @doc Free DMA-coherent memory (3-arg version)
dma_free_coherent(_Dev, _Size, Ptr) ->
    log_kapi_call(dma_free_coherent, [_Dev, _Size, Ptr]),
    kfree(Ptr).

%% @doc Free DMA-coherent memory (4-arg version with DMA handle)
%% TODO: Track DMA handles properly when implementing real DMA subsystem
dma_free_coherent(_Dev, _Size, Ptr, _DmaHandle) ->
    log_kapi_call(dma_free_coherent, [_Dev, _Size, Ptr, _DmaHandle]),
    %% Ignore DmaHandle for now - delegate to 3-arg version
    kfree(Ptr).

%% ==================================================================
%% Logging
%% ==================================================================

%% @doc Kernel print - route to BEAM logger
%% FINDING 6 FIX: Handle Linux-style format strings (%s, %d, etc.) that crash io_lib:format
printk(Fmt, Args) ->
    %% Don't log this call itself to avoid infinite recursion
    Message = try
        io_lib:format(Fmt, Args)
    catch
        error:badarg ->
            %% Fallback: Linux format string not compatible with Erlang - output safely
            try
                io_lib:format("~tp ~tp", [Fmt, Args])
            catch
                _:_ ->
                    "[printk: format error]"
            end
    end,
    logger:notice("[KAPI printk] ~s", [Message]),
    ok.

%% @doc Device error log
dev_err(Dev, Msg) ->
    logger:error("[KAPI dev_err] ~p: ~s", [Dev, Msg]),
    ok.

%% @doc Device warning log
dev_warn(Dev, Msg) ->
    logger:warning("[KAPI dev_warn] ~p: ~s", [Dev, Msg]),
    ok.

%% @doc Device info log
dev_info(Dev, Msg) ->
    logger:info("[KAPI dev_info] ~p: ~s", [Dev, Msg]),
    ok.

%% @doc Device debug log
dev_dbg(Dev, Msg) ->
    logger:debug("[KAPI dev_dbg] ~p: ~s", [Dev, Msg]),
    ok.

%% ==================================================================
%% Synchronization
%% ==================================================================

%% @doc Initialize spinlock
spin_lock_init(Lock) ->
    log_kapi_call(spin_lock_init, [Lock]),
    %% Real implementation: register lock in ETS, return lock ID
    {ok, make_ref()}.

%% @doc Acquire spinlock
%% Real implementation: use gen_server call for serialization
spin_lock(Lock) ->
    log_kapi_call(spin_lock, [Lock]),
    %% TODO: Call lock manager gen_server
    ok.

%% @doc Release spinlock
spin_unlock(Lock) ->
    log_kapi_call(spin_unlock, [Lock]),
    %% TODO: Call lock manager gen_server
    ok.

%% @doc Acquire spinlock with interrupt save (2-arg version)
%% Returns {ok, SavedFlags} where SavedFlags is 0 (single-threaded BEAM)
spin_lock_irqsave(Lock, _Flags) ->
    log_kapi_call(spin_lock_irqsave, [Lock, _Flags]),
    %% TODO: Call lock manager gen_server
    %% Return 0 as saved flags since we're single-threaded
    {ok, 0}.

%% @doc Release spinlock with interrupt restore (2-arg version)
spin_unlock_irqrestore(Lock, _Flags) ->
    log_kapi_call(spin_unlock_irqrestore, [Lock, _Flags]),
    %% TODO: Call lock manager gen_server
    ok.

%% @doc Initialize mutex
mutex_init(Mutex) ->
    log_kapi_call(mutex_init, [Mutex]),
    {ok, make_ref()}.

%% @doc Acquire mutex
%% Real implementation: gen_server:call to mutex manager
mutex_lock(Mutex) ->
    log_kapi_call(mutex_lock, [Mutex]),
    ok.

%% @doc Release mutex
mutex_unlock(Mutex) ->
    log_kapi_call(mutex_unlock, [Mutex]),
    ok.

%% @doc Initialize wait queue
init_waitqueue_head(Queue) ->
    log_kapi_call(init_waitqueue_head, [Queue]),
    %% Real implementation: create waiting process registry
    {ok, make_ref()}.

%% @doc Wake up waiting processes
wake_up(Queue) ->
    log_kapi_call(wake_up, [Queue]),
    %% Real implementation: send message to all waiting processes
    ok.

%% @doc Wake up interruptible waiting processes
wake_up_interruptible(Queue) ->
    log_kapi_call(wake_up_interruptible, [Queue]),
    ok.

%% ==================================================================
%% PCI
%% ==================================================================

%% @doc Register PCI driver
pci_register_driver(Driver) ->
    log_kapi_call(pci_register_driver, [Driver]),
    %% Real implementation: register with vbeam_pci, trigger probe callbacks
    0. %% Success

%% @doc Unregister PCI driver
pci_unregister_driver(Driver) ->
    log_kapi_call(pci_unregister_driver, [Driver]),
    ok.

%% @doc Enable PCI device
pci_enable_device(Dev) ->
    log_kapi_call(pci_enable_device, [Dev]),
    %% Real implementation: enable MMIO/IO access, set up BARs
    0. %% Success

%% @doc Disable PCI device
pci_disable_device(Dev) ->
    log_kapi_call(pci_disable_device, [Dev]),
    ok.

%% @doc Set PCI bus mastering
pci_set_master(Dev) ->
    log_kapi_call(pci_set_master, [Dev]),
    %% Real implementation: set DMA enable bit in PCI config
    ok.

%% @doc Get PCI resource start address
pci_resource_start(Dev, Bar) ->
    log_kapi_call(pci_resource_start, [Dev, Bar]),
    %% Real implementation: query vbeam_pci for BAR base address
    16#F800_0000 + (Bar * 16#0010_0000). %% Fake address

%% @doc Get PCI resource length
pci_resource_len(Dev, Bar) ->
    log_kapi_call(pci_resource_len, [Dev, Bar]),
    %% Real implementation: query vbeam_pci for BAR size
    16#0010_0000. %% 1MB default

%% @doc Map PCI BAR to kernel virtual address
pci_iomap(Dev, Bar, MaxLen) ->
    log_kapi_call(pci_iomap, [Dev, Bar, MaxLen]),
    %% Real implementation: map to BEAM binary, track mapping
    {ok, make_ref()}.

%% @doc Unmap PCI BAR
pci_iounmap(Dev, Addr) ->
    log_kapi_call(pci_iounmap, [Dev, Addr]),
    ok.

%% ==================================================================
%% Interrupts
%% ==================================================================

%% @doc Request interrupt line
%% Real implementation: register IRQ handler process, route BEAM messages
%% NOTE: Currently returns stub indicator - real IRQ registration not implemented
request_irq(Irq, Handler, Flags, Name, Dev) ->
    log_kapi_call(request_irq, [Irq, Handler, Flags, Name, Dev]),
    logger:warning("[KAPI] request_irq: stub implementation - IRQ not actually registered"),
    %% TODO: Register Handler with vbeam_irq manager
    Ref = make_ref(),
    {ok, {stub, Ref}}. %% Stub indicator

%% @doc Request threaded interrupt line (with separate thread handler)
%% TODO: Thread handler not supported yet - log warning and delegate to request_irq
request_threaded_irq(Irq, Handler, ThreadFn, Flags, Name, Dev) ->
    log_kapi_call(request_threaded_irq, [Irq, Handler, ThreadFn, Flags, Name, Dev]),
    case ThreadFn of
        undefined -> ok;
        _ -> logger:warning("[KAPI] request_threaded_irq: thread handler ignored (not implemented)")
    end,
    request_irq(Irq, Handler, Flags, Name, Dev).

%% @doc Free interrupt line
free_irq(Irq, Dev) ->
    log_kapi_call(free_irq, [Irq, Dev]),
    ok.

%% @doc Enable interrupt
enable_irq(Irq) ->
    log_kapi_call(enable_irq, [Irq]),
    ok.

%% @doc Disable interrupt
disable_irq(Irq) ->
    log_kapi_call(disable_irq, [Irq]),
    ok.

%% ==================================================================
%% Workqueues/Timers
%% ==================================================================

%% @doc Schedule work
%% Real implementation: spawn BEAM process to execute work function
schedule_work(Work) ->
    log_kapi_call(schedule_work, [Work]),
    %% TODO: spawn_link fun that calls Work function
    true. %% Queued

%% @doc Queue work on specific workqueue
queue_work(Queue, Work) ->
    log_kapi_call(queue_work, [Queue, Work]),
    schedule_work(Work).

%% @doc Modify timer expiration
%% Real implementation: erlang:send_after
%% NOTE: Caller must remove timer ref when handling {timer_expired, Timer, Gen} message
%% by calling remove_timer_ref(Timer) to prevent ref leak
mod_timer(Timer, ExpiresJiffies) ->
    log_kapi_call(mod_timer, [Timer, ExpiresJiffies]),
    %% BUG 7 FIX: Validate timeout range (0 to 24h)
    MaxTimeout = 86400000, % 24 hours in ms
    case is_integer(ExpiresJiffies) andalso ExpiresJiffies >= 0 andalso ExpiresJiffies =< MaxTimeout of
        true ->
            %% Cancel old timer if exists and delete ref
            case get_timer_ref(Timer) of
                undefined -> ok;
                {OldTRef, _Gen} ->
                    %% FINDING 7 FIX: Extract actual timer ref from tuple
                    erlang:cancel_timer(OldTRef),
                    %% LOW FIX: Flush any stale messages from cancelled timer
                    receive
                        {timer_expired, Timer, _OldGen} -> ok
                    after 0 -> ok
                    end,
                    remove_timer_ref(Timer);
                OldTRef when is_reference(OldTRef) ->
                    %% Legacy format (bare ref) - still handle it
                    erlang:cancel_timer(OldTRef),
                    receive
                        {timer_expired, Timer, _OldGen} -> ok
                    after 0 -> ok
                    end,
                    remove_timer_ref(Timer)
            end,
            %% Jiffies conversion: 1 jiffy = 1ms on most systems
            TimeoutMs = ExpiresJiffies,
            Self = self(),
            %% LOW FIX: Add generation counter to timer messages
            Gen = erlang:unique_integer([monotonic, positive]),
            TRef = erlang:send_after(TimeoutMs, Self, {timer_expired, Timer, Gen}),
            %% BUG 2 FIX: Check if store_timer_ref succeeded
            case store_timer_ref(Timer, {TRef, Gen}) of
                ok ->
                    0; %% Success
                {error, timer_table_full} ->
                    %% Storage failed - cancel the timer immediately
                    erlang:cancel_timer(TRef),
                    -1 %% Failure
            end;
        false ->
            -1 %% Invalid timeout
    end.

%% @doc Delete timer
%% FINDING 9 FIX: Handle both {TRef, Gen} and legacy bare TRef formats
del_timer(Timer) ->
    log_kapi_call(del_timer, [Timer]),
    %% BUG 7 FIX: Actually cancel the timer
    case get_timer_ref(Timer) of
        undefined ->
            0; %% Was not active
        {TRef, _Gen} ->
            erlang:cancel_timer(TRef),
            %% LOW FIX: Flush any stale messages from cancelled timer
            receive
                {timer_expired, Timer, _OldGen} -> ok
            after 0 -> ok
            end,
            remove_timer_ref(Timer),
            1; %% Was active, now cancelled
        TRef when is_reference(TRef) ->
            %% Legacy bare timer ref (not wrapped in tuple)
            erlang:cancel_timer(TRef),
            receive
                {timer_expired, Timer, _OldGen} -> ok
            after 0 -> ok
            end,
            remove_timer_ref(Timer),
            1 %% Was active, now cancelled
    end.

%% @doc Clean up timer ref after timer fires (call when handling {timer_expired, Timer, Gen})
%% This prevents timer ref leaks from one-shot timers
%% Generation-aware: only removes if generation matches (prevents stale timer from clearing new timer)
timer_fired(Timer, Gen) ->
    case get_timer_ref(Timer) of
        {_TRef, StoredGen} when StoredGen =:= Gen ->
            %% Generation matches - this is the current timer firing
            remove_timer_ref(Timer),
            ok;
        _ ->
            %% Generation mismatch or no timer - stale fire, ignore
            ok
    end.

%% @doc Legacy 1-arg version - always remove (unsafe but backward compatible)
timer_fired(Timer) ->
    remove_timer_ref(Timer),
    ok.

%% ==================================================================
%% DMA
%% ==================================================================

%% @doc Map single buffer for DMA
dma_map_single(Dev, Ptr, Size, Direction) ->
    log_kapi_call(dma_map_single, [Dev, Ptr, Size, Direction]),
    %% Real implementation: pin memory, return physical address
    DmaAddr = 16#E000_0000 + erlang:phash2(Ptr, 16#0FFF_FFFF),
    DmaAddr.

%% @doc Unmap single DMA buffer
dma_unmap_single(Dev, DmaAddr, Size, Direction) ->
    log_kapi_call(dma_unmap_single, [Dev, DmaAddr, Size, Direction]),
    ok.

%% @doc Set DMA mask
dma_set_mask(Dev, Mask) ->
    log_kapi_call(dma_set_mask, [Dev, Mask]),
    %% Real implementation: validate device can address Mask bits
    0. %% Success

%% ==================================================================
%% Network Subsystem
%% ==================================================================

%% @doc Allocate ethernet device
alloc_etherdev(PrivSize) ->
    log_kapi_call(alloc_etherdev, [PrivSize]),
    %% Real implementation: create net_device structure, allocate private data
    NetDev = #{
        type => etherdev,
        priv_size => PrivSize,
        priv => make_ref(),
        mac => <<16#52, 16#54, 16#00, 16#12, 16#34, 16#56>>, %% Default MAC
        mtu => 1500,
        state => down
    },
    {ok, NetDev}.

%% @doc Register network device
register_netdev(NetDev) ->
    log_kapi_call(register_netdev, [NetDev]),
    %% Real implementation: register with vbeam_net, assign interface name
    0. %% Success

%% @doc Unregister network device
unregister_netdev(NetDev) ->
    log_kapi_call(unregister_netdev, [NetDev]),
    ok.

%% @doc Start network queue
netif_start_queue(NetDev) ->
    log_kapi_call(netif_start_queue, [NetDev]),
    ok.

%% @doc Stop network queue
netif_stop_queue(NetDev) ->
    log_kapi_call(netif_stop_queue, [NetDev]),
    ok.

%% @doc Wake network queue
netif_wake_queue(NetDev) ->
    log_kapi_call(netif_wake_queue, [NetDev]),
    ok.

%% @doc Receive packet
%% Real implementation: pass sk_buff to network stack
netif_rx(Skb) ->
    log_kapi_call(netif_rx, [Skb]),
    0. %% NET_RX_SUCCESS

%% @doc Schedule NAPI poll
napi_schedule(Napi) ->
    log_kapi_call(napi_schedule, [Napi]),
    %% Real implementation: trigger poll in BEAM process
    ok.

%% @doc Complete NAPI poll
napi_complete(Napi) ->
    log_kapi_call(napi_complete, [Napi]),
    ok.

%% ==================================================================
%% Internal Helpers
%% ==================================================================

%% @doc Log kernel API call for debugging

%% Timer reference storage using ETS (atomic, thread-safe)
init_timer_storage() ->
    case ets:whereis(?TIMER_TABLE) of
        undefined ->
            init();
        _ ->
            ok
    end.

store_timer_ref(Timer, TRef) ->
    init_timer_storage(),
    %% BUG 10 FIX: Check cardinality before inserting
    case ets:info(?TIMER_TABLE, size) of
        Size when Size >= 9500 ->
            %% Approaching capacity - prune dead process entries first
            prune_dead_timers(),
            %% Re-check after prune
            case ets:info(?TIMER_TABLE, size) of
                NewSize when NewSize < 10000 ->
                    %% BUG 11 FIX: Key by {self(), Timer} to avoid namespace collision
                    OwnerPid = self(),
                    ets:insert(?TIMER_TABLE, {{OwnerPid, Timer}, TRef}),
                    ok;
                _ ->
                    {error, timer_table_full}
            end;
        _ ->
            %% BUG 11 FIX: Key by {self(), Timer} to avoid namespace collision
            OwnerPid = self(),
            ets:insert(?TIMER_TABLE, {{OwnerPid, Timer}, TRef}),
            ok
    end.

get_timer_ref(Timer) ->
    init_timer_storage(),
    %% BUG 11 FIX: Lookup by {self(), Timer}
    case ets:lookup(?TIMER_TABLE, {self(), Timer}) of
        [{{_Pid, Timer}, TRefAndGen}] -> TRefAndGen;
        [] -> undefined
    end.

remove_timer_ref(Timer) ->
    init_timer_storage(),
    %% BUG 11 FIX: Delete by {self(), Timer}
    ets:delete(?TIMER_TABLE, {self(), Timer}),
    ok.

%% Prune timer refs where owner process is dead
prune_dead_timers() ->
    init_timer_storage(),
    AllKeys = ets:match(?TIMER_TABLE, {{'$1', '_'}, '_'}),
    DeadPids = [Pid || [Pid] <- AllKeys, not is_process_alive(Pid)],
    lists:foreach(fun(DeadPid) ->
        ets:match_delete(?TIMER_TABLE, {{DeadPid, '_'}, '_'})
    end, DeadPids),
    ok.

log_kapi_call(Function, Args) ->
    %% Use debug level to avoid spam, can enable selectively
    logger:debug("[KAPI] ~p(~p)", [Function, Args]),
    ok.
