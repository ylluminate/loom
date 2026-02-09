%%% @doc Tests for vbeam_gdt_idt module.
%%% @end
-module(vbeam_gdt_idt_test).

-export([run_all/0]).

run_all() ->
    io:format("Running vbeam_gdt_idt tests...~n"),

    Tests = [
        fun test_gdt_data_size/0,
        fun test_gdt_entry_structure/0,
        fun test_gdt_load_code_exists/0,
        fun test_idt_data_size/0,
        fun test_idt_entry_structure/0,
        fun test_idt_load_code_exists/0,
        fun test_exception_stubs_exist/0,
        fun test_timer_isr_exists/0
    ],

    Results = [run_test(Test) || Test <- Tests],

    Passed = length([ok || ok <- Results]),
    Failed = length(Results) - Passed,

    io:format("~n=== Test Summary ===~n"),
    io:format("Passed: ~w~n", [Passed]),
    io:format("Failed: ~w~n", [Failed]),

    case Failed of
        0 ->
            io:format("~nAll tests passed!~n"),
            ok;
        _ ->
            io:format("~nSome tests failed.~n"),
            {error, failed}
    end.

run_test(Test) ->
    try
        Test(),
        io:format("  [PASS] ~p~n", [element(2, erlang:fun_info(Test, name))]),
        ok
    catch
        Class:Reason:Stack ->
            io:format("  [FAIL] ~p: ~p:~p~n", [
                element(2, erlang:fun_info(Test, name)),
                Class,
                Reason
            ]),
            io:format("    Stack: ~p~n", [Stack]),
            fail
    end.

%%====================================================================
%% Test Cases
%%====================================================================

test_gdt_data_size() ->
    Data = vbeam_gdt_idt:gdt_data(),

    %% GDT should be:
    %% - 5 entries * 8 bytes = 40 bytes
    %% - GDTR structure = 10 bytes (2 byte limit + 8 byte base)
    %% Total = 50 bytes
    ExpectedSize = 50,
    ActualSize = byte_size(Data),

    case ActualSize of
        ExpectedSize -> ok;
        _ -> error({size_mismatch, expected, ExpectedSize, got, ActualSize})
    end.

test_gdt_entry_structure() ->
    Data = vbeam_gdt_idt:gdt_data(),

    %% Entry 0 should be all zeros (null descriptor)
    <<Entry0:8/binary, _Rest/binary>> = Data,
    case Entry0 of
        <<0:64>> -> ok;
        _ -> error({entry0_not_null, Entry0})
    end,

    %% Entry 1 should have access byte 0x9A (code, ring 0)
    %% Skip 8 bytes (Entry 0), then extract Entry 1
    <<_:64, Entry1:8/binary, _/binary>> = Data,
    %% Access byte is at offset 5 in each GDT entry
    <<_:16, _:16, _:8, AccessByte1:8, _:16>> = Entry1,
    case AccessByte1 of
        16#9A -> ok;
        _ -> error({entry1_access_byte, expected, 16#9A, got, AccessByte1})
    end,

    %% Entry 2 should have access byte 0x92 (data, ring 0)
    <<_:128, Entry2:8/binary, _/binary>> = Data,
    <<_:16, _:16, _:8, AccessByte2:8, _:16>> = Entry2,
    case AccessByte2 of
        16#92 -> ok;
        _ -> error({entry2_access_byte, expected, 16#92, got, AccessByte2})
    end,

    %% Entry 3 should have access byte 0xFA (code, ring 3)
    <<_:192, Entry3:8/binary, _/binary>> = Data,
    <<_:16, _:16, _:8, AccessByte3:8, _:16>> = Entry3,
    case AccessByte3 of
        16#FA -> ok;
        _ -> error({entry3_access_byte, expected, 16#FA, got, AccessByte3})
    end,

    %% Entry 4 should have access byte 0xF2 (data, ring 3)
    <<_:256, Entry4:8/binary, _/binary>> = Data,
    <<_:16, _:16, _:8, AccessByte4:8, _:16>> = Entry4,
    case AccessByte4 of
        16#F2 -> ok;
        _ -> error({entry4_access_byte, expected, 16#F2, got, AccessByte4})
    end,

    %% GDTR should have limit 39
    <<_:320, Limit:16/little, _:64>> = Data,
    case Limit of
        39 -> ok;
        _ -> error({gdtr_limit, expected, 39, got, Limit})
    end.

test_gdt_load_code_exists() ->
    Code = vbeam_gdt_idt:gdt_load_code(16#100000),

    %% Should be non-empty binary
    case byte_size(Code) > 0 of
        true -> ok;
        false -> error(gdt_load_code_empty)
    end.

test_idt_data_size() ->
    Data = vbeam_gdt_idt:idt_data(16#200000),

    %% IDT should be 256 entries * 16 bytes = 4096 bytes
    ExpectedSize = 4096,
    ActualSize = byte_size(Data),

    case ActualSize of
        ExpectedSize -> ok;
        _ -> error({size_mismatch, expected, ExpectedSize, got, ActualSize})
    end.

test_idt_entry_structure() ->
    ISRBase = 16#200000,
    Data = vbeam_gdt_idt:idt_data(ISRBase),

    %% Entry 0 should point to ISRBase + 0
    <<Entry0:16/binary, _Rest/binary>> = Data,
    <<OffsetLow0:16/little, Selector0:16/little, IST0:8, TypeAttr0:8,
      OffsetMid0:16/little, OffsetHigh0:32/little, Reserved0:32/little>> = Entry0,

    %% Reconstruct offset
    Offset0 = OffsetLow0 + (OffsetMid0 bsl 16) + (OffsetHigh0 bsl 32),
    case Offset0 of
        ISRBase -> ok;
        _ -> error({entry0_offset, expected, ISRBase, got, Offset0})
    end,

    %% Check selector (should be 0x08 for kernel CS)
    case Selector0 of
        16#08 -> ok;
        _ -> error({entry0_selector, expected, 16#08, got, Selector0})
    end,

    %% Check type/attributes (0x8E = interrupt gate, ring 0, present)
    case TypeAttr0 of
        16#8E -> ok;
        _ -> error({entry0_type_attr, expected, 16#8E, got, TypeAttr0})
    end,

    %% Check IST (should be 0)
    case IST0 of
        0 -> ok;
        _ -> error({entry0_ist, expected, 0, got, IST0})
    end,

    %% Check reserved (should be 0)
    case Reserved0 of
        0 -> ok;
        _ -> error({entry0_reserved, expected, 0, got, Reserved0})
    end,

    %% Entry 32 should point to ISRBase + (32 * 10)
    <<_:512/binary, Entry32:16/binary, _/binary>> = Data,
    <<OffsetLow32:16/little, _:16, _:8, _:8,
      OffsetMid32:16/little, OffsetHigh32:32/little, _:32>> = Entry32,

    Offset32 = OffsetLow32 + (OffsetMid32 bsl 16) + (OffsetHigh32 bsl 32),
    Expected32 = ISRBase + (32 * 10),
    case Offset32 of
        Expected32 -> ok;
        _ -> error({entry32_offset, expected, Expected32, got, Offset32})
    end.

test_idt_load_code_exists() ->
    Code = vbeam_gdt_idt:idt_load_code(16#300000),

    %% Should be non-empty binary
    case byte_size(Code) > 0 of
        true -> ok;
        false -> error(idt_load_code_empty)
    end.

test_exception_stubs_exist() ->
    Stubs = vbeam_gdt_idt:exception_stubs(),

    %% Should be non-empty binary
    case byte_size(Stubs) > 0 of
        true -> ok;
        false -> error(exception_stubs_empty)
    end,

    %% Should contain at least 34 stubs (0-31 exceptions + timer + generic)
    %% Each stub is 10 bytes, plus common handler
    MinSize = 34 * 10,
    case byte_size(Stubs) >= MinSize of
        true -> ok;
        false -> error({exception_stubs_too_small, expected_min, MinSize, got, byte_size(Stubs)})
    end.

test_timer_isr_exists() ->
    ISR = vbeam_gdt_idt:timer_isr(),

    %% Should be a binary (may be empty if included in exception_stubs)
    case is_binary(ISR) of
        true -> ok;
        false -> error(timer_isr_not_binary)
    end.
