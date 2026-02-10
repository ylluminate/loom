#!/usr/bin/env escript
%% @doc Quick verification script for nucleus.efi
%% Checks PE32+ header integrity: MZ magic, PE signature, machine type.
%%
%% Usage:
%%   ./tools/verify_nucleus.escript                    # verify boot/nucleus.efi
%%   ./tools/verify_nucleus.escript path/to/nucleus.efi

-mode(compile).

main(Args) ->
    Path = case Args of
        [P | _] -> P;
        []      -> "boot/nucleus.efi"
    end,
    case file:read_file(Path) of
        {error, enoent} ->
            io:format("~c[31m✗~c[0m ~s not found. Run: make nucleus~n", [27, 27, Path]),
            halt(1);
        {ok, Bin} ->
            io:format("=== Nucleus Verification ===~n~n"),
            verify_file_info(Path, Bin),
            verify_pe_header(Bin),
            verify_machine_type(Bin),
            io:format("~n=== Verification passed! ===~n~n"),
            io:format("To test in QEMU:~n  ./tools/test_nucleus_qemu.escript~n")
    end.

verify_file_info(Path, Bin) ->
    io:format("[1/3] File info:~n"),
    io:format("  ~s~n", [Path]),
    io:format("  Size: ~B bytes~n~n", [byte_size(Bin)]).

verify_pe_header(Bin) ->
    io:format("[2/3] PE header check:~n"),
    %% DOS header: first 2 bytes must be "MZ" (0x4D5A)
    case Bin of
        <<16#4D, 16#5A, _/binary>> ->
            io:format("  ~c[32m✓~c[0m DOS header magic (MZ) found~n", [27, 27]);
        _ ->
            io:format("  ~c[31m✗~c[0m Invalid DOS header~n", [27, 27]),
            halt(1)
    end,
    %% PE signature at offset 64: "PE\0\0" (0x50450000)
    case Bin of
        <<_:64/binary, 16#50, 16#45, 16#00, 16#00, _/binary>> ->
            io:format("  ~c[32m✓~c[0m PE signature (PE\\0\\0) found at offset 64~n", [27, 27]);
        _ ->
            io:format("  ~c[31m✗~c[0m Invalid PE signature~n", [27, 27]),
            halt(1)
    end,
    io:format("~n").

verify_machine_type(Bin) ->
    io:format("[3/3] Machine type:~n"),
    %% Machine type at offset 68, 2 bytes little-endian
    %% x86-64 = 0x8664
    <<_:68/binary, MachLow, MachHigh, _/binary>> = Bin,
    Machine = MachHigh bsl 8 bor MachLow,
    case Machine of
        16#8664 ->
            io:format("  ~c[32m✓~c[0m Machine type: 0x8664 (x86-64)~n", [27, 27]);
        Other ->
            io:format("  ~c[31m✗~c[0m Unexpected machine type: 0x~4.16.0B~n", [27, 27, Other]),
            halt(1)
    end,
    io:format("~n").
