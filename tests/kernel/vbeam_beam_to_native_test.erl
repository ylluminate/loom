-module(vbeam_beam_to_native_test).

-export([run_all/0, test/0]).

%% Wrapper for make test-kernel compatibility
test() -> run_all().

%%% ==========================================================================
%%% Test Runner
%%% ==========================================================================

run_all() ->
    io:format("~n=== BEAM-to-Native Translator Tests ===~n~n"),

    Tests = [
        fun test_serial_putchar_code/0,
        fun test_serial_puts_code/0,
        fun test_translate_simple_return/0,
        fun test_translate_allocate_deallocate/0,
        fun test_translate_move/0,
        fun test_output_format/0,
        fun test_translate_function_basic/0,
        fun test_helper_code_sizes/0,
        fun test_entry_offset_calculation/0
    ],

    Results = lists:map(fun(Test) -> run_test(Test) end, Tests),
    Passed = length([ok || ok <- Results]),
    Failed = length(Results) - Passed,

    io:format("~n=== Results ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),

    case Failed of
        0 -> io:format("~nAll tests passed!~n"), halt(0);
        _ -> io:format("~nSome tests failed.~n"), halt(1)
    end.

run_test(Test) ->
    TestName = element(2, erlang:fun_info(Test, name)),
    io:format("Running ~s... ", [TestName]),
    try
        Test(),
        io:format("PASS~n"),
        ok
    catch
        Class:Reason:Stack ->
            io:format("FAIL~n  ~p:~p~n  ~p~n", [Class, Reason, Stack]),
            fail
    end.

%%% ==========================================================================
%%% Tests
%%% ==========================================================================

test_serial_putchar_code() ->
    Code = vbeam_beam_to_native:serial_putchar_code(),
    true = is_binary(Code),
    true = byte_size(Code) > 0,

    %% Should be reasonably sized (< 100 bytes)
    true = byte_size(Code) < 100,

    %% Should contain characteristic bytes (port 0x3FD and 0x3F8)
    %% mov edx, 0x3FD = BA FD 03 00 00
    %% mov edx, 0x3F8 = BA F8 03 00 00
    true = binary:match(Code, <<16#BA, 16#FD, 16#03>>) =/= nomatch,
    true = binary:match(Code, <<16#BA, 16#F8, 16#03>>) =/= nomatch,

    %% Should contain ret instruction
    true = binary:match(Code, <<16#C3>>) =/= nomatch,

    ok.

test_serial_puts_code() ->
    Code = vbeam_beam_to_native:serial_puts_code(),
    true = is_binary(Code),
    true = byte_size(Code) > 0,

    %% Should be reasonably sized (< 100 bytes)
    true = byte_size(Code) < 100,

    %% Should contain lodsb instruction (AC)
    true = binary:match(Code, <<16#AC>>) =/= nomatch,

    %% Should contain test rcx, rcx (48 85 C9) for length-bounded loop
    true = binary:match(Code, <<16#48, 16#85, 16#C9>>) =/= nomatch,

    %% Should contain ret instruction
    true = binary:match(Code, <<16#C3>>) =/= nomatch,

    ok.

test_translate_simple_return() ->
    %% Translate a simple return opcode
    {ok, Code} = vbeam_beam_to_native:translate_function([return], #{}),
    true = is_binary(Code),

    %% Should be exactly 1 byte (ret = C3)
    1 = byte_size(Code),
    <<16#C3>> = Code,

    ok.

test_translate_allocate_deallocate() ->
    %% Test stack frame operations
    Opcodes = [
        {allocate, 2, 0},    %% Allocate 2 stack slots (16 bytes)
        {deallocate, 2}      %% Deallocate 2 stack slots
    ],

    {ok, Code} = vbeam_beam_to_native:translate_function(Opcodes, #{}),
    true = is_binary(Code),
    true = byte_size(Code) > 0,

    %% Should contain sub rsp, 16 (48 83 EC 10)
    true = binary:match(Code, <<16#48, 16#83, 16#EC, 16#10>>) =/= nomatch,

    %% Should contain add rsp, 16 (48 83 C4 10)
    true = binary:match(Code, <<16#48, 16#83, 16#C4, 16#10>>) =/= nomatch,

    ok.

test_translate_move() ->
    %% Test register moves
    Opcodes = [
        {move, {x, 0}, {x, 1}},   %% mov rbx, rax
        {move, {x, 1}, {x, 0}}    %% mov rax, rbx
    ],

    {ok, Code} = vbeam_beam_to_native:translate_function(Opcodes, #{}),
    true = is_binary(Code),

    %% Should contain mov rbx, rax (48 89 C3)
    true = binary:match(Code, <<16#48, 16#89, 16#C3>>) =/= nomatch,

    %% Should contain mov rax, rbx (48 89 D8)
    true = binary:match(Code, <<16#48, 16#89, 16#D8>>) =/= nomatch,

    ok.

test_output_format() ->
    %% Test that translate_beam returns proper map structure
    %% We'll create a minimal test by translating function opcodes
    Opcodes = [{allocate, 1, 0}, return],

    {ok, Code} = vbeam_beam_to_native:translate_function(Opcodes, #{}),
    true = is_binary(Code),

    %% The translate_beam/1 function requires a .beam file, so we test the
    %% internal structure indirectly. The key is that the return map should have:
    %% #{code => Binary, data => Binary, entry => Offset}

    ok.

test_translate_function_basic() ->
    %% Test translating a complete function structure
    Opcodes = [
        {label, 1},
        {func_info, {atom, test}, {atom, hello}, 0},
        {label, 2},
        {allocate, 0, 0},
        {move, {integer, 42}, {x, 0}},
        {deallocate, 0},
        return
    ],

    {ok, Code} = vbeam_beam_to_native:translate_function(Opcodes, #{}),
    true = is_binary(Code),
    true = byte_size(Code) > 0,

    %% Should contain ret instruction at the end
    LastByte = binary:last(Code),
    16#C3 = LastByte,  % ret

    ok.

test_helper_code_sizes() ->
    %% Verify helper functions have consistent sizes
    Putchar = vbeam_beam_to_native:serial_putchar_code(),
    Puts = vbeam_beam_to_native:serial_puts_code(),

    PutcharSize = byte_size(Putchar),
    PutsSize = byte_size(Puts),

    %% Both should be non-empty and reasonable
    true = PutcharSize > 10,
    true = PutcharSize < 50,
    true = PutsSize > 10,
    true = PutsSize < 50,

    io:format("  putchar: ~p bytes, puts: ~p bytes", [PutcharSize, PutsSize]),

    ok.

test_entry_offset_calculation() ->
    %% Verify that entry offset calculation makes sense
    %% Entry should be after helper functions

    Putchar = vbeam_beam_to_native:serial_putchar_code(),
    Puts = vbeam_beam_to_native:serial_puts_code(),

    ExpectedEntryOffset = byte_size(Putchar) + byte_size(Puts),

    %% Entry offset should be sum of helper sizes
    true = ExpectedEntryOffset > 0,

    io:format("  entry offset: ~p bytes", [ExpectedEntryOffset]),

    ok.

%%% ==========================================================================
%%% Note: test_translate_hello_world and test_string_embedding would require
%%% actual .beam files to test properly. These are deferred until we have
%%% a V hello world .beam file to work with.
%%% ==========================================================================
