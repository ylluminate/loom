%%% @doc Tests for vbeam_io_server
-module(vbeam_io_server_test).

-export([run_all/0]).

-define(TEST_FB_INFO, #{
    base => 16#B8000,
    width => 1024,
    height => 768,
    stride => 1024 * 4
}).

%%% ----------------------------------------------------------------------------
%%% Test Runner
%%% ----------------------------------------------------------------------------

run_all() ->
    io:format("Running vbeam_io_server tests...~n"),

    Tests = [
        fun test_start_serial_only/0,
        fun test_io_request_put_chars/0,
        fun test_io_request_put_chars_mfa/0,
        fun test_direct_write/0,
        fun test_log_buffer/0,
        fun test_cursor_advance/0,
        fun test_newline/0,
        fun test_set_group_leader/0,
        fun test_unknown_request/0,
        fun test_latin1_encoding/0
    ],

    Results = [run_test(T) || T <- Tests],

    Passed = length([ok || ok <- Results]),
    Failed = length(Results) - Passed,

    io:format("~n=== Test Results ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),

    case Failed of
        0 -> io:format("~nAll tests passed!~n");
        _ -> io:format("~nSome tests failed.~n")
    end.

run_test(TestFun) ->
    TestName = erlang:fun_info_mfa(TestFun),
    io:format("~nRunning ~p... ", [TestName]),

    %% Trap exits to prevent test crashes from propagating
    process_flag(trap_exit, true),

    try
        TestFun(),
        io:format("PASS~n"),
        ok
    catch
        Class:Reason:Stack ->
            io:format("FAIL~n"),
            io:format("  Error: ~p:~p~n", [Class, Reason]),
            io:format("  Stack: ~p~n", [Stack]),
            fail
    after
        %% Clean up any remaining messages
        flush_mailbox(),
        %% Ensure server is stopped if it exists
        case whereis(vbeam_io_server) of
            undefined -> ok;
            Pid when is_pid(Pid) ->
                catch gen_server:stop(Pid),
                timer:sleep(10)
        end,
        %% Restore trap_exit
        process_flag(trap_exit, false)
    end.

flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.

%%% ----------------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------------

test_start_serial_only() ->
    {ok, Pid} = vbeam_io_server:start_link(#{serial => true}),
    true = is_process_alive(Pid),
    gen_server:stop(Pid),
    ok.

test_io_request_put_chars() ->
    %% Don't link to the server to avoid EXIT messages
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Send io_request with put_chars
    Ref = make_ref(),
    Pid ! {io_request, self(), Ref, {put_chars, unicode, <<"test">>}},

    %% Wait for io_reply
    receive
        {io_reply, Ref, ok} -> ok;
        {io_reply, Ref, Other} -> throw({unexpected_reply, Other});
        Other -> throw({unexpected_message, Other})
    after 1000 ->
        throw(timeout)
    end,

    %% Verify log buffer contains "test"
    LogBuf = vbeam_io_server:get_log(),
    case binary:match(LogBuf, <<"test">>) of
        {_, _} -> ok;
        nomatch -> throw({log_missing, LogBuf})
    end,

    gen_server:stop(Pid),
    ok.

test_io_request_put_chars_mfa() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Send io_request with put_chars MFA
    Ref = make_ref(),
    Pid ! {io_request, self(), Ref,
           {put_chars, unicode, io_lib, format, ["Value: ~p", [42]]}},

    %% Wait for io_reply
    receive
        {io_reply, Ref, ok} -> ok;
        {io_reply, Ref, Other} -> throw({unexpected_reply, Other});
        Other -> throw({unexpected_message, Other})
    after 1000 ->
        throw(timeout)
    end,

    %% Verify log buffer contains "Value: 42"
    LogBuf = vbeam_io_server:get_log(),
    case binary:match(LogBuf, <<"Value: 42">>) of
        {_, _} -> ok;
        nomatch -> throw({log_missing, LogBuf})
    end,

    gen_server:stop(Pid),
    ok.

test_direct_write() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    vbeam_io_server:write("direct write test"),

    %% Give cast time to process
    timer:sleep(50),

    LogBuf = vbeam_io_server:get_log(),
    case binary:match(LogBuf, <<"direct write test">>) of
        {_, _} -> ok;
        nomatch -> throw({log_missing, LogBuf})
    end,

    gen_server:stop(Pid),
    ok.

test_log_buffer() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Write multiple times
    vbeam_io_server:write("line1\n"),
    vbeam_io_server:write("line2\n"),
    vbeam_io_server:writeln("line3"),

    %% Give casts time to process
    timer:sleep(50),

    LogBuf = vbeam_io_server:get_log(),

    %% Verify all lines present
    case binary:match(LogBuf, <<"line1">>) of
        {_, _} -> ok;
        nomatch -> throw({missing_line1, LogBuf})
    end,
    case binary:match(LogBuf, <<"line2">>) of
        {_, _} -> ok;
        nomatch -> throw({missing_line2, LogBuf})
    end,
    case binary:match(LogBuf, <<"line3">>) of
        {_, _} -> ok;
        nomatch -> throw({missing_line3, LogBuf})
    end,

    gen_server:stop(Pid),
    ok.

test_cursor_advance() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{
        serial => false,
        log => false,
        framebuffer => ?TEST_FB_INFO
    }, []),

    %% Write some characters and verify cursor advances
    vbeam_io_server:write("abc"),

    %% Give cast time to process
    timer:sleep(50),

    %% We can't easily verify cursor position without internal state access,
    %% but at least verify no crash
    true = is_process_alive(Pid),

    gen_server:stop(Pid),
    ok.

test_newline() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{
        serial => false,
        log => true,
        framebuffer => ?TEST_FB_INFO
    }, []),

    %% Write text with newlines
    vbeam_io_server:write("line1\nline2\nline3\n"),

    %% Give cast time to process
    timer:sleep(50),

    LogBuf = vbeam_io_server:get_log(),

    %% Verify newlines preserved
    Lines = binary:split(LogBuf, <<"\n">>, [global]),
    case length(Lines) >= 3 of
        true -> ok;
        false -> throw({insufficient_lines, Lines})
    end,

    gen_server:stop(Pid),
    ok.

test_set_group_leader() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Set as group leader
    OldGL = group_leader(),
    true = vbeam_io_server:set_group_leader(),

    %% Verify group leader changed
    Pid = group_leader(),

    %% Use io:format, which should route through our server
    io:format("routed test~n"),

    %% Give time to process
    timer:sleep(50),

    LogBuf = vbeam_io_server:get_log(),
    case binary:match(LogBuf, <<"routed test">>) of
        {_, _} -> ok;
        nomatch -> throw({routing_failed, LogBuf})
    end,

    %% Restore group leader
    group_leader(OldGL, self()),

    gen_server:stop(Pid),
    ok.

test_unknown_request() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Send unknown io_request
    Ref = make_ref(),
    Pid ! {io_request, self(), Ref, {unknown_request, foo}},

    %% Should get error reply
    receive
        {io_reply, Ref, {error, _}} -> ok;
        {io_reply, Ref, Other} -> throw({expected_error, got, Other});
        Other -> throw({unexpected_message, Other})
    after 1000 ->
        throw(timeout)
    end,

    gen_server:stop(Pid),
    ok.

test_latin1_encoding() ->
    {ok, Pid} = gen_server:start({local, vbeam_io_server}, vbeam_io_server, #{serial => false, log => true}, []),

    %% Send io_request with latin1 encoding
    Ref = make_ref(),
    Pid ! {io_request, self(), Ref, {put_chars, latin1, "latin1 test"}},

    %% Wait for io_reply
    receive
        {io_reply, Ref, ok} -> ok;
        {io_reply, Ref, Other} -> throw({unexpected_reply, Other});
        Other -> throw({unexpected_message, Other})
    after 1000 ->
        throw(timeout)
    end,

    %% Verify log buffer
    LogBuf = vbeam_io_server:get_log(),
    case binary:match(LogBuf, <<"latin1 test">>) of
        {_, _} -> ok;
        nomatch -> throw({log_missing, LogBuf})
    end,

    gen_server:stop(Pid),
    ok.
