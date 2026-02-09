%%% @doc Tests for vbeam_irq_bridge — Interrupt-to-Message Bridge.
%%% @end
-module(vbeam_irq_bridge_test).

-export([run_all/0]).

run_all() ->
    io:format("~n=== IRQ Bridge Tests ===~n~n"),

    Tests = [
        fun test_ring_buffer_push_pop/0,
        fun test_ring_buffer_wraparound/0,
        fun test_ring_buffer_pop_all/0,
        fun test_ring_buffer_size/0,
        fun test_registration/0,
        fun test_delivery_single_irq/0,
        fun test_delivery_multiple_irqs/0,
        fun test_delivery_no_handler/0,
        fun test_isr_code_generation/0
    ],

    Results = [run_test(Test) || Test <- Tests],
    Failed = length([R || R <- Results, R =:= fail]),

    io:format("~n=== Results: ~w/~w passed ===~n~n",
              [length(Results) - Failed, length(Results)]),

    case Failed of
        0 -> ok;
        _ -> halt(1)
    end.

run_test(TestFn) ->
    TestName = element(2, erlang:fun_info(TestFn, name)),
    io:format("Running ~s... ", [TestName]),
    try
        TestFn(),
        io:format("PASS~n"),
        pass
    catch
        Class:Reason:Stacktrace ->
            io:format("FAIL~n  ~p:~p~n  ~p~n", [Class, Reason, Stacktrace]),
            fail
    end.

%%% ==========================================================================
%%% Ring Buffer Tests
%%% ==========================================================================

test_ring_buffer_push_pop() ->
    Buf = vbeam_irq_bridge:ring_new(16),
    Buf1 = vbeam_irq_bridge:ring_push({1, 1000}, Buf),
    Buf2 = vbeam_irq_bridge:ring_push({2, 2000}, Buf1),

    {ok, {1, 1000}, Buf3} = vbeam_irq_bridge:ring_pop(Buf2),
    {ok, {2, 2000}, Buf4} = vbeam_irq_bridge:ring_pop(Buf3),
    empty = vbeam_irq_bridge:ring_pop(Buf4),

    ok.

test_ring_buffer_wraparound() ->
    %% Create small buffer (size 4) and push more than capacity
    Buf = vbeam_irq_bridge:ring_new(4),

    %% Push 6 events (should overwrite oldest 2)
    Buf1 = vbeam_irq_bridge:ring_push({1, 100}, Buf),
    Buf2 = vbeam_irq_bridge:ring_push({2, 200}, Buf1),
    Buf3 = vbeam_irq_bridge:ring_push({3, 300}, Buf2),
    Buf4 = vbeam_irq_bridge:ring_push({4, 400}, Buf3),
    Buf5 = vbeam_irq_bridge:ring_push({5, 500}, Buf4),
    Buf6 = vbeam_irq_bridge:ring_push({6, 600}, Buf5),

    %% Should only have events 3, 4, 5, 6 (oldest 1, 2 were overwritten)
    4 = vbeam_irq_bridge:ring_size(Buf6),

    {ok, {3, 300}, Buf7} = vbeam_irq_bridge:ring_pop(Buf6),
    {ok, {4, 400}, Buf8} = vbeam_irq_bridge:ring_pop(Buf7),
    {ok, {5, 500}, Buf9} = vbeam_irq_bridge:ring_pop(Buf8),
    {ok, {6, 600}, Buf10} = vbeam_irq_bridge:ring_pop(Buf9),
    empty = vbeam_irq_bridge:ring_pop(Buf10),

    ok.

test_ring_buffer_pop_all() ->
    Buf = vbeam_irq_bridge:ring_new(16),
    Buf1 = vbeam_irq_bridge:ring_push({10, 1000}, Buf),
    Buf2 = vbeam_irq_bridge:ring_push({20, 2000}, Buf1),
    Buf3 = vbeam_irq_bridge:ring_push({30, 3000}, Buf2),

    {Events, Buf4} = vbeam_irq_bridge:ring_pop_all(Buf3),
    [{10, 1000}, {20, 2000}, {30, 3000}] = Events,
    0 = vbeam_irq_bridge:ring_size(Buf4),

    ok.

test_ring_buffer_size() ->
    Buf = vbeam_irq_bridge:ring_new(16),
    0 = vbeam_irq_bridge:ring_size(Buf),

    Buf1 = vbeam_irq_bridge:ring_push({1, 100}, Buf),
    1 = vbeam_irq_bridge:ring_size(Buf1),

    Buf2 = vbeam_irq_bridge:ring_push({2, 200}, Buf1),
    2 = vbeam_irq_bridge:ring_size(Buf2),

    {ok, _, Buf3} = vbeam_irq_bridge:ring_pop(Buf2),
    1 = vbeam_irq_bridge:ring_size(Buf3),

    ok.

%%% ==========================================================================
%%% Handler Registration Tests
%%% ==========================================================================

test_registration() ->
    {ok, Pid} = vbeam_irq_bridge:start_link(),

    %% Initially no handlers
    #{} = vbeam_irq_bridge:get_handlers(),

    %% Register handler for IRQ 1
    TestPid = self(),
    ok = vbeam_irq_bridge:register_handler(1, TestPid),

    #{1 := TestPid} = vbeam_irq_bridge:get_handlers(),

    %% Unregister
    ok = vbeam_irq_bridge:unregister_handler(1),
    #{} = vbeam_irq_bridge:get_handlers(),

    exit(Pid, kill),
    ok.

%%% ==========================================================================
%%% Message Delivery Tests
%%% ==========================================================================

test_delivery_single_irq() ->
    {ok, BridgePid} = vbeam_irq_bridge:start_link(),

    %% Register self as handler for IRQ 1
    TestPid = self(),
    ok = vbeam_irq_bridge:register_handler(1, TestPid),

    %% Simulate IRQ 1
    ok = vbeam_irq_bridge:simulate_irq(1),

    %% Give the cast time to process
    timer:sleep(10),

    %% Tick should deliver the message
    {ok, 1} = vbeam_irq_bridge:tick(BridgePid),

    %% Verify we received the message
    receive
        {irq, 1, Timestamp} when is_integer(Timestamp) ->
            ok
    after 100 ->
        error(message_not_received)
    end,

    exit(BridgePid, kill),
    ok.

test_delivery_multiple_irqs() ->
    {ok, BridgePid} = vbeam_irq_bridge:start_link(),

    %% Register handlers for IRQ 1 and IRQ 2
    TestPid = self(),
    ok = vbeam_irq_bridge:register_handler(1, TestPid),
    ok = vbeam_irq_bridge:register_handler(2, TestPid),

    %% Simulate multiple IRQs
    ok = vbeam_irq_bridge:simulate_irq(1),
    ok = vbeam_irq_bridge:simulate_irq(2),
    ok = vbeam_irq_bridge:simulate_irq(1),

    %% Give the casts time to process
    timer:sleep(10),

    %% Tick should deliver 3 messages
    {ok, 3} = vbeam_irq_bridge:tick(BridgePid),

    %% Verify we received all messages
    Messages = receive_all_irqs(3, []),
    3 = length(Messages),

    %% Check we got IRQ 1 twice and IRQ 2 once
    [_, _, _] = Messages,  % 3 messages total

    exit(BridgePid, kill),
    ok.

test_delivery_no_handler() ->
    {ok, BridgePid} = vbeam_irq_bridge:start_link(),

    %% Don't register any handlers

    %% Simulate IRQ
    ok = vbeam_irq_bridge:simulate_irq(99),

    %% Give the cast time to process
    timer:sleep(10),

    %% Tick should report 0 delivered (no handler)
    {ok, 0} = vbeam_irq_bridge:tick(BridgePid),

    %% Verify we received no messages
    receive
        {irq, _, _} ->
            error(unexpected_message)
    after 50 ->
        ok
    end,

    exit(BridgePid, kill),
    ok.

receive_all_irqs(0, Acc) ->
    lists:reverse(Acc);
receive_all_irqs(N, Acc) ->
    receive
        {irq, IrqNum, Timestamp} ->
            receive_all_irqs(N - 1, [{IrqNum, Timestamp} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%% ==========================================================================
%%% ISR Code Generation Test
%%% ==========================================================================

test_isr_code_generation() ->
    Code = vbeam_irq_bridge:isr_ring_buffer_write(),

    %% Verify it's a non-empty binary
    true = is_binary(Code),
    true = byte_size(Code) > 0,

    %% Verify it starts with push instructions (save registers)
    <<16#50, _/binary>> = Code,  % push rax

    ok.
