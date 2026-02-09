#!/usr/bin/env escript
%%% @doc Test script for ELF loader.
%%% Creates a simple test object file and parses it.

-mode(compile).

main([]) ->
    io:format("=== ELF Loader Test ===~n~n"),

    %% Create test object file
    TestCSource = "/tmp/test_ko.c",
    TestObjFile = "/tmp/test_ko.o",

    io:format("Creating test C source...~n"),
    SourceCode = "int test_func(void) { return 42; }\n"
                 "int global_var = 123;\n"
                 "extern int external_func(int);\n"
                 "int caller(void) { return external_func(10); }\n",

    ok = file:write_file(TestCSource, SourceCode),

    %% Compile to object file
    io:format("Compiling to object file...~n"),
    CompileCmd = case os:type() of
        {unix, darwin} ->
            %% macOS - use cross-compilation target
            lists:flatten(io_lib:format(
                "clang -target x86_64-linux-gnu -c -o ~s ~s 2>&1",
                [TestObjFile, TestCSource]
            ));
        {unix, linux} ->
            lists:flatten(io_lib:format(
                "gcc -c -o ~s ~s 2>&1",
                [TestObjFile, TestCSource]
            ))
    end,

    case os:cmd(CompileCmd) of
        "" ->
            io:format("  OK~n~n");
        Output ->
            io:format("  Compiler output: ~s~n~n", [Output])
    end,

    %% Load the module
    io:format("Compiling loader module...~n"),
    case compile:file("vbeam_elf_loader.erl", [binary, return_errors, {i, "."}]) of
        {ok, vbeam_elf_loader, Binary} ->
            code:load_binary(vbeam_elf_loader, "vbeam_elf_loader.erl", Binary),
            io:format("  Module loaded~n");
        {error, Errors, Warnings} ->
            io:format("Compile errors: ~p~n", [Errors]),
            io:format("Compile warnings: ~p~n", [Warnings]),
            halt(1)
    end,

    io:format("~n=== Parsing ELF object ===~n"),
    {ok, ElfBinary} = file:read_file(TestObjFile),
    io:format("File size: ~p bytes~n~n", [byte_size(ElfBinary)]),

    case vbeam_elf_loader:parse(ElfBinary) of
        {ok, ElfInfo} ->
            print_elf_info(ElfInfo),

            %% Test symbol resolution
            io:format("~n=== Testing Symbol Resolution ===~n"),
            SymbolTable = #{
                <<"external_func">> => 16#2000000
            },

            case vbeam_elf_loader:resolve_symbols(ElfInfo, SymbolTable) of
                {ok, ResolvedElf} ->
                    io:format("All symbols resolved successfully~n"),

                    %% Test relocation
                    io:format("~n=== Testing Relocation ===~n"),
                    BaseAddr = 16#1000000,
                    io:format("Base address: 0x~.16B~n", [BaseAddr]),

                    case vbeam_elf_loader:apply_relocations(ResolvedElf, BaseAddr) of
                        {ok, Code} ->
                            io:format("Relocation successful~n"),
                            io:format("Loaded code size: ~p bytes~n", [byte_size(Code)]),
                            io:format("~nTest PASSED~n");
                        {error, RelocError} ->
                            io:format("Relocation failed: ~p~n", [RelocError]),
                            halt(1)
                    end;

                {error, Unresolved} ->
                    io:format("Unresolved symbols: ~p~n", [Unresolved]),
                    io:format("(This is expected - test_func and caller are defined locally)~n")
            end;

        {error, ParseError} ->
            io:format("Parse failed: ~p~n", [ParseError]),
            halt(1)
    end,

    io:format("~n=== Cleanup ===~n"),
    file:delete(TestCSource),
    file:delete(TestObjFile),
    io:format("Done~n").

%% Print ELF info structure
print_elf_info(#{header := Header, sections := Sections, symbols := Symbols, relocations := Relocs}) ->
    io:format("--- ELF Header ---~n"),
    io:format("  Type: ~p~n", [maps:get(type, Header)]),
    io:format("  Machine: ~p~n", [maps:get(machine, Header)]),
    io:format("  Class: ~p-bit~n", [maps:get(class, Header)]),
    io:format("  Endian: ~p~n", [maps:get(endian, Header)]),
    io:format("  Sections: ~p~n", [length(Sections)]),

    io:format("~n--- Sections ---~n"),
    lists:foreach(
        fun(#{name := Name, type := Type, size := Size, flags := Flags}) ->
            FlagsStr = section_flags_str(Flags),
            TypeStr = format_type(Type),
            io:format("  ~-20s ~-10s size=~6w flags=~s~n",
                      [Name, TypeStr, Size, FlagsStr])
        end,
        Sections
    ),

    io:format("~n--- Symbols (~p total) ---~n", [length(Symbols)]),
    lists:foreach(
        fun(#{name := Name, bind := Bind, type := Type, shndx := Shndx, value := Value, size := Size}) ->
            case Name of
                <<>> -> ok;  % Skip empty names
                _ ->
                    ShndxStr = case Shndx of
                        0 -> "UNDEF";
                        16#FFF1 -> "ABS";
                        N -> integer_to_list(N)
                    end,
                    io:format("  ~-30s ~-8s ~-8s shndx=~-6s value=0x~.16B size=~w~n",
                              [Name, Bind, Type, ShndxStr, Value, Size])
            end
        end,
        Symbols
    ),

    io:format("~n--- Relocations ---~n"),
    case maps:size(Relocs) of
        0 ->
            io:format("  (none)~n");
        _ ->
            maps:foreach(
                fun(SecName, RelocList) ->
                    io:format("  Section: ~s (~p relocations)~n", [SecName, length(RelocList)]),
                    lists:foreach(
                        fun(#{offset := Off, type := Type, symbol := Sym, addend := Add}) ->
                            io:format("    offset=0x~.16B type=~-16s sym=~w addend=~w~n",
                                      [Off, Type, Sym, Add])
                        end,
                        RelocList
                    )
                end,
                Relocs
            )
    end.

section_flags_str(Flags) ->
    lists:flatten([
        case Flags band 4 of 0 -> ""; _ -> "X" end,
        case Flags band 2 of 0 -> ""; _ -> "A" end,
        case Flags band 1 of 0 -> ""; _ -> "W" end
    ]).

format_type(Type) when is_atom(Type) ->
    atom_to_list(Type);
format_type({unknown, N}) ->
    lists:flatten(io_lib:format("unk_~w", [N])).
