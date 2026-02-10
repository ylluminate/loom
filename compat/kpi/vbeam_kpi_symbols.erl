-module(vbeam_kpi_symbols).
-export([resolve/1, get_all_symbols/0, is_supported/1]).

%% @doc Resolve Linux kernel symbol name to BEAM {Module, Function, Arity}
%% Used by the .ko loader to resolve imports
resolve(SymbolName) ->
    case symbol_map(SymbolName) of
        undefined ->
            logger:warning("[KAPI Symbols] Unresolved symbol: ~s", [SymbolName]),
            {error, {unresolved_symbol, SymbolName}};
        {M, F, A} ->
            {ok, {M, F, A}}
    end.

%% @doc Get all available kernel API symbols
get_all_symbols() ->
    maps:keys(all_symbols()).

%% @doc Check if symbol is supported
is_supported(SymbolName) ->
    maps:is_key(SymbolName, all_symbols()).

%% ==================================================================
%% Symbol Mapping
%% ==================================================================

%% @doc Map symbol name to {Module, Function, Arity}
symbol_map(SymbolName) ->
    maps:get(SymbolName, all_symbols(), undefined).

%% @doc All available kernel symbols
%% Format: "symbol_name" => {module, function, arity}
all_symbols() ->
    #{
        %% ============================================================
        %% Memory Management
        %% ============================================================
        "kmalloc" => {vbeam_linuxkpi, kmalloc, 2},
        "__kmalloc" => {vbeam_linuxkpi, kmalloc, 2},
        "kfree" => {vbeam_linuxkpi, kfree, 1},
        "vmalloc" => {vbeam_linuxkpi, vmalloc, 1},
        "__vmalloc" => {vbeam_linuxkpi, vmalloc, 1},
        "vfree" => {vbeam_linuxkpi, vfree, 1},
        "kzalloc" => {vbeam_linuxkpi, kzalloc, 2},
        "dma_alloc_coherent" => {vbeam_linuxkpi, dma_alloc_coherent, 4},
        %% BUG 5 FIX: dma_free_coherent has 4 args (dev, size, vaddr, dma_handle)
        "dma_free_coherent" => {vbeam_linuxkpi, dma_free_coherent, 4},

        %% ============================================================
        %% Logging
        %% ============================================================
        "printk" => {vbeam_linuxkpi, printk, 2},
        "_printk" => {vbeam_linuxkpi, printk, 2},
        "dev_err" => {vbeam_linuxkpi, dev_err, 2},
        "dev_warn" => {vbeam_linuxkpi, dev_warn, 2},
        "dev_info" => {vbeam_linuxkpi, dev_info, 2},
        "dev_dbg" => {vbeam_linuxkpi, dev_dbg, 2},

        %% ============================================================
        %% Synchronization - Spinlocks
        %% ============================================================
        "spin_lock_init" => {vbeam_linuxkpi, spin_lock_init, 1},
        "_raw_spin_lock" => {vbeam_linuxkpi, spin_lock, 1},
        "_raw_spin_unlock" => {vbeam_linuxkpi, spin_unlock, 1},
        "spin_lock" => {vbeam_linuxkpi, spin_lock, 1},
        "spin_unlock" => {vbeam_linuxkpi, spin_unlock, 1},
        "spin_lock_irqsave" => {vbeam_linuxkpi, spin_lock_irqsave, 2},
        "spin_unlock_irqrestore" => {vbeam_linuxkpi, spin_unlock_irqrestore, 2},

        %% ============================================================
        %% Synchronization - Mutexes
        %% ============================================================
        "mutex_init" => {vbeam_linuxkpi, mutex_init, 1},
        "__mutex_init" => {vbeam_linuxkpi, mutex_init, 1},
        "mutex_lock" => {vbeam_linuxkpi, mutex_lock, 1},
        "mutex_unlock" => {vbeam_linuxkpi, mutex_unlock, 1},

        %% ============================================================
        %% Synchronization - Wait Queues
        %% ============================================================
        "init_waitqueue_head" => {vbeam_linuxkpi, init_waitqueue_head, 1},
        "__init_waitqueue_head" => {vbeam_linuxkpi, init_waitqueue_head, 1},
        "wake_up" => {vbeam_linuxkpi, wake_up, 1},
        "__wake_up" => {vbeam_linuxkpi, wake_up, 1},
        "wake_up_interruptible" => {vbeam_linuxkpi, wake_up_interruptible, 1},

        %% ============================================================
        %% PCI
        %% ============================================================
        "pci_register_driver" => {vbeam_linuxkpi, pci_register_driver, 1},
        "__pci_register_driver" => {vbeam_linuxkpi, pci_register_driver, 1},
        "pci_unregister_driver" => {vbeam_linuxkpi, pci_unregister_driver, 1},
        "pci_enable_device" => {vbeam_linuxkpi, pci_enable_device, 1},
        "pci_disable_device" => {vbeam_linuxkpi, pci_disable_device, 1},
        "pci_set_master" => {vbeam_linuxkpi, pci_set_master, 1},
        "pci_resource_start" => {vbeam_linuxkpi, pci_resource_start, 2},
        "pci_resource_len" => {vbeam_linuxkpi, pci_resource_len, 2},
        "pci_iomap" => {vbeam_linuxkpi, pci_iomap, 3},
        "pci_iounmap" => {vbeam_linuxkpi, pci_iounmap, 2},

        %% ============================================================
        %% Interrupts
        %% ============================================================
        "request_irq" => {vbeam_linuxkpi, request_irq, 5},
        %% BUG 4 FIX: request_threaded_irq has 6 args (irq, handler, thread_fn, flags, name, dev)
        "request_threaded_irq" => {vbeam_linuxkpi, request_threaded_irq, 6},
        "free_irq" => {vbeam_linuxkpi, free_irq, 2},
        "enable_irq" => {vbeam_linuxkpi, enable_irq, 1},
        "disable_irq" => {vbeam_linuxkpi, disable_irq, 1},

        %% ============================================================
        %% Workqueues
        %% ============================================================
        "schedule_work" => {vbeam_linuxkpi, schedule_work, 1},
        "queue_work" => {vbeam_linuxkpi, queue_work, 2},

        %% ============================================================
        %% Timers
        %% ============================================================
        "mod_timer" => {vbeam_linuxkpi, mod_timer, 2},
        "del_timer" => {vbeam_linuxkpi, del_timer, 1},
        "del_timer_sync" => {vbeam_linuxkpi, del_timer, 1},

        %% ============================================================
        %% DMA
        %% ============================================================
        "dma_map_single" => {vbeam_linuxkpi, dma_map_single, 4},
        "dma_unmap_single" => {vbeam_linuxkpi, dma_unmap_single, 4},
        "dma_set_mask" => {vbeam_linuxkpi, dma_set_mask, 2},
        "dma_set_coherent_mask" => {vbeam_linuxkpi, dma_set_mask, 2},

        %% ============================================================
        %% Network Subsystem
        %% ============================================================
        "alloc_etherdev" => {vbeam_linuxkpi, alloc_etherdev, 1},
        "alloc_etherdev_mq" => {vbeam_linuxkpi, alloc_etherdev, 1}, %% Multi-queue variant
        "register_netdev" => {vbeam_linuxkpi, register_netdev, 1},
        "unregister_netdev" => {vbeam_linuxkpi, unregister_netdev, 1},
        "netif_start_queue" => {vbeam_linuxkpi, netif_start_queue, 1},
        "netif_stop_queue" => {vbeam_linuxkpi, netif_stop_queue, 1},
        "netif_wake_queue" => {vbeam_linuxkpi, netif_wake_queue, 1},
        "netif_rx" => {vbeam_linuxkpi, netif_rx, 1},
        "napi_schedule" => {vbeam_linuxkpi, napi_schedule, 1},
        "__napi_schedule" => {vbeam_linuxkpi, napi_schedule, 1},
        "napi_complete" => {vbeam_linuxkpi, napi_complete, 1},
        "napi_complete_done" => {vbeam_linuxkpi, napi_complete, 1}
    }.

%% Dead function removed: symbols_by_category/1 was unused and not exported
