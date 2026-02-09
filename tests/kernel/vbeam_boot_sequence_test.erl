%%% @doc Tests for vbeam_boot_sequence module.
-module(vbeam_boot_sequence_test).

-export([run_all/0]).

run_all() ->
    io:format("Running vbeam_boot_sequence tests...~n"),

    Tests = [
        fun test_boot_code_returns_binary/0,
        fun test_boot_data_returns_binary/0,
        fun test_boot_data_layout_has_keys/0,
        fun test_gdt_data_matches/0,
        fun test_page_tables_match/0,
        fun test_boot_code_size_reasonable/0,
        fun test_boot_data_contains_strings/0,
        fun test_layout_offsets_valid/0
    ],

    Results = [run_test(Test) || Test <- Tests],
    Passed = length([ok || ok <- Results]),
    Failed = length(Results) - Passed,

    io:format("~n=== Results ===~n"),
    io:format("Passed: ~w/~w~n", [Passed, length(Results)]),
    io:format("Failed: ~w~n", [Failed]),

    case Failed of
        0 -> io:format("~c All tests passed!~n", [16#2713]);
        _ -> io:format("~c Some tests failed~n", [16#2717])
    end.

run_test(TestFun) ->
    TestName = atom_to_list(element(2, erlang:fun_info(TestFun, name))),
    try
        TestFun(),
        io:format("~c ~s~n", [16#2713, TestName]),
        ok
    catch
        Error:Reason:Stack ->
            io:format("~c ~s~n", [16#2717, TestName]),
            io:format("  Error: ~p:~p~n", [Error, Reason]),
            io:format("  Stack: ~p~n", [Stack]),
            error
    end.

%%====================================================================
%% Tests
%%====================================================================

test_boot_code_returns_binary() ->
    Config = default_config(),
    Code = vbeam_boot_sequence:boot_code(Config),
    true = is_binary(Code),
    true = byte_size(Code) > 0.

test_boot_data_returns_binary() ->
    Config = default_config(),
    Data = vbeam_boot_sequence:boot_data(Config),
    true = is_binary(Data),
    true = byte_size(Data) > 0.

test_boot_data_layout_has_keys() ->
    Config = default_config(),
    Layout = vbeam_boot_sequence:boot_data_layout(Config),

    %% Check required keys exist
    true = maps:is_key(gdt_offset, Layout),
    true = maps:is_key(gdt_size, Layout),
    true = maps:is_key(idt_offset, Layout),
    true = maps:is_key(idt_size, Layout),
    true = maps:is_key(page_tables_offset, Layout),
    true = maps:is_key(page_tables_size, Layout),
    true = maps:is_key(strings, Layout),

    %% Check strings is a list
    #{strings := Strings} = Layout,
    true = is_list(Strings),
    true = length(Strings) > 0.

test_gdt_data_matches() ->
    Config = default_config(),
    Data = vbeam_boot_sequence:boot_data(Config),
    Layout = vbeam_boot_sequence:boot_data_layout(Config),

    #{gdt_offset := GDTOffset, gdt_size := GDTSize} = Layout,

    %% Extract GDT portion from boot data
    <<_:GDTOffset/binary, GDTFromBoot:GDTSize/binary, _/binary>> = Data,

    %% Compare with direct GDT generation
    GDTDirect = vbeam_gdt_idt:gdt_data(),

    GDTFromBoot = GDTDirect.

test_page_tables_match() ->
    Config = default_config(),
    Data = vbeam_boot_sequence:boot_data(Config),
    Layout = vbeam_boot_sequence:boot_data_layout(Config),

    #{page_tables_offset := PTOffset, page_tables_size := PTSize} = Layout,

    %% Extract page table portion from boot data
    <<_:PTOffset/binary, PTFromBoot:PTSize/binary, _/binary>> = Data,

    %% Compare with direct page table generation
    PTDirect = vbeam_paging:page_tables(4),

    PTFromBoot = PTDirect.

test_boot_code_size_reasonable() ->
    Config = default_config(),
    Code = vbeam_boot_sequence:boot_code(Config),
    Size = byte_size(Code),

    %% Code should be non-empty but less than 4KB
    true = Size > 0,
    true = Size < 4096,

    io:format("  Boot code size: ~w bytes~n", [Size]).

test_boot_data_contains_strings() ->
    Config = default_config(),
    Data = vbeam_boot_sequence:boot_data(Config),

    %% Check that boot data contains our status strings
    RequiredStrings = [
        <<"[BOOT] ExitBootServices...">>,
        <<"[BOOT] GDT loaded">>,
        <<"[BOOT] IDT loaded">>,
        <<"[BOOT] Paging configured">>,
        <<"[BOOT] Stack ready">>,
        <<"[BOOT] BEAM kernel ready">>
    ],

    lists:foreach(fun(Str) ->
        case binary:match(Data, Str) of
            nomatch -> error({string_not_found, Str});
            _ -> ok
        end
    end, RequiredStrings).

test_layout_offsets_valid() ->
    Config = default_config(),
    Layout = vbeam_boot_sequence:boot_data_layout(Config),

    #{
        gdt_offset := GDTOff,
        gdt_size := GDTSize,
        idt_offset := IDTOff,
        idt_size := IDTSize,
        page_tables_offset := PTOff,
        page_tables_size := PTSize
    } = Layout,

    %% Offsets should be sequential and non-overlapping
    true = GDTOff =:= 0,
    true = IDTOff =:= GDTOff + GDTSize,
    true = PTOff =:= IDTOff + IDTSize,

    %% Sizes should be reasonable
    true = GDTSize > 0,
    true = IDTSize > 0,
    true = PTSize > 0,

    io:format("  Layout: GDT=~w bytes, IDT=~w bytes, PT=~w bytes~n",
              [GDTSize, IDTSize, PTSize]).

%%====================================================================
%% Helpers
%%====================================================================

default_config() ->
    #{
        gdt_base => 16#200000,
        idt_base => 16#201000,
        page_tables_base => 16#300000,
        heap_base => 16#400000,
        heap_size => 16#1000000,
        stack_base => 16#1400000
    }.
