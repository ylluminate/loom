#!/usr/bin/env escript
%%% -*- erlang -*-
%%! -mode(compile)
%%
%% Render bitmap fonts for bare-metal OS.
%%
%% Supports:
%%   - BDF (Bitmap Distribution Format) font files as input
%%   - Reading existing generated font modules for validation/re-export
%%
%% Generates Erlang module containing pre-rendered glyphs for ASCII 32-126.
%% Each glyph is 16 bytes (8 pixels wide, 16 rows, MSB = leftmost pixel).

-mode(compile).

main(Args) ->
    try
        case parse_args(Args) of
            {error, Msg} ->
                io:format(standard_error, "Error: ~s~n", [Msg]),
                usage(),
                halt(1);
            Opts ->
                execute(Opts)
        end
    catch
        error:Reason:Stack ->
            io:format(standard_error, "Fatal error: ~p~n~p~n", [Reason, Stack]),
            halt(1)
    end.

usage() ->
    io:format(
        "Usage: render_font_bitmap.escript [OPTIONS] INPUT~n"
        "~n"
        "INPUT:~n"
        "  BDF font file (e.g., font.bdf)~n"
        "  Existing Erlang font module (e.g., vbeam_font_8x16.erl)~n"
        "~n"
        "OPTIONS:~n"
        "  -o OUTPUT.erl      Output path (default: font_NAME.erl)~n"
        "  --validate         Just validate existing module~n"
        "  --preview          Print ASCII art preview of glyphs~n"
        "  --help             Show this help~n"
        "~n"
        "EXAMPLES:~n"
        "  render_font_bitmap.escript -o boot/fonts/font.erl font.bdf~n"
        "  render_font_bitmap.escript --preview boot/fonts/vbeam_font_8x16.erl~n"
        "  render_font_bitmap.escript --validate boot/fonts/vbeam_font_8x16.erl~n"
    ).

parse_args(Args) ->
    parse_args(Args, #{mode => generate, preview => false}).

parse_args([], #{input := _} = Opts) ->
    Opts;
parse_args([], _) ->
    {error, "Missing input file"};
parse_args(["--help" | _], _) ->
    usage(),
    halt(0);
parse_args(["-o", Output | Rest], Opts) ->
    parse_args(Rest, Opts#{output => Output});
parse_args(["--validate" | Rest], Opts) ->
    parse_args(Rest, Opts#{mode => validate});
parse_args(["--preview" | Rest], Opts) ->
    parse_args(Rest, Opts#{preview => true});
parse_args([Input | Rest], Opts) ->
    parse_args(Rest, Opts#{input => Input}).

execute(#{input := Input, mode := Mode, preview := Preview} = Opts) ->
    io:format("Reading: ~s~n", [Input]),

    case detect_input_type(Input) of
        bdf ->
            GlyphData = parse_bdf(Input),
            maybe_preview(GlyphData, Preview),
            maybe_generate(GlyphData, Opts, Mode);
        erlang_module ->
            GlyphData = read_erlang_module(Input),
            maybe_preview(GlyphData, Preview),
            maybe_validate(GlyphData, Mode)
    end,

    halt(0).

detect_input_type(Path) ->
    case filename:extension(Path) of
        ".bdf" -> bdf;
        ".erl" -> erlang_module;
        _ ->
            %% Try reading first line to detect
            case file:read_file(Path) of
                {ok, <<"STARTFONT", _/binary>>} -> bdf;
                {ok, <<"-module", _/binary>>} -> erlang_module;
                {ok, <<"%%", _/binary>>} -> erlang_module;
                _ -> bdf  %% Default to BDF
            end
    end.

%% ============================================================================
%% BDF Parser
%% ============================================================================

parse_bdf(Path) ->
    {ok, Content} = file:read_file(Path),
    Lines = binary:split(Content, <<"\n">>, [global]),
    Glyphs = extract_glyphs(Lines),

    %% Ensure we have all 95 ASCII chars (32-126)
    GlyphMap = maps:from_list(Glyphs),

    %% Build ordered binary: 16 bytes per char, ASCII 32-126
    AllBytes = lists:flatten([
        case maps:get(CharCode, GlyphMap, undefined) of
            undefined ->
                %% Missing glyph - use blank (16 zeros)
                io:format("Warning: Missing glyph for char ~w, using blank~n", [CharCode]),
                lists:duplicate(16, 0);
            GlyphBytes ->
                GlyphBytes
        end
        || CharCode <- lists:seq(32, 126)
    ]),

    Binary = list_to_binary(AllBytes),
    io:format("Parsed ~w glyphs from BDF, total ~w bytes~n",
              [maps:size(GlyphMap), byte_size(Binary)]),
    Binary.

extract_glyphs(Lines) ->
    extract_glyphs(Lines, [], undefined, []).

extract_glyphs([], Acc, _, _) ->
    lists:reverse(Acc);
extract_glyphs([<<"STARTCHAR ", _/binary>> | Rest], Acc, _, _) ->
    %% Start of a character definition
    extract_glyphs(Rest, Acc, undefined, []);
extract_glyphs([<<"ENCODING ", EncodingBin/binary>> | Rest], Acc, _, BitmapLines) ->
    Encoding = binary_to_integer(string:trim(EncodingBin)),
    extract_glyphs(Rest, Acc, Encoding, BitmapLines);
extract_glyphs([<<"BITMAP">> | Rest], Acc, Encoding, _) ->
    %% Start of bitmap data
    extract_glyphs(Rest, Acc, Encoding, []);
extract_glyphs([<<"ENDCHAR">> | Rest], Acc, Encoding, BitmapLines) when Encoding =/= undefined ->
    %% End of character - process bitmap
    GlyphBytes = process_bdf_bitmap(lists:reverse(BitmapLines)),
    extract_glyphs(Rest, [{Encoding, GlyphBytes} | Acc], undefined, []);
extract_glyphs([Line | Rest], Acc, Encoding, BitmapLines) when Encoding =/= undefined ->
    %% Accumulate bitmap lines (hex data between BITMAP and ENDCHAR)
    Trimmed = string:trim(Line),
    case Trimmed of
        <<>> ->
            extract_glyphs(Rest, Acc, Encoding, BitmapLines);
        _ ->
            extract_glyphs(Rest, Acc, Encoding, [Trimmed | BitmapLines])
    end;
extract_glyphs([_ | Rest], Acc, Encoding, BitmapLines) ->
    %% Skip other lines
    extract_glyphs(Rest, Acc, Encoding, BitmapLines).

process_bdf_bitmap(HexLines) ->
    %% BDF bitmap: each line is a hex value representing one row
    %% We need exactly 16 rows for 8x16 font
    %% If fewer rows, pad with zeros
    %% If more rows, crop to 16
    Rows = [parse_hex_byte(Line) || Line <- HexLines],

    %% Ensure 16 rows
    case length(Rows) of
        N when N < 16 ->
            Rows ++ lists:duplicate(16 - N, 0);
        N when N > 16 ->
            lists:sublist(Rows, 16);
        _ ->
            Rows
    end.

parse_hex_byte(Bin) ->
    %% BDF hex can be 2 or 4 chars (byte or word)
    %% We want the first byte (leftmost 8 pixels)
    Trimmed = string:trim(Bin),
    case byte_size(Trimmed) of
        2 ->
            binary_to_integer(Trimmed, 16);
        4 ->
            %% Take first 2 chars
            <<B1, B2, _/binary>> = Trimmed,
            binary_to_integer(<<B1, B2>>, 16);
        _ ->
            %% Fallback - try parsing as is
            try
                binary_to_integer(Trimmed, 16)
            catch _:_ ->
                0
            end
    end.

%% ============================================================================
%% Erlang Module Reader
%% ============================================================================

read_erlang_module(Path) ->
    {ok, Content} = file:read_file(Path),

    %% Extract the binary literal from font_data() function
    %% Look for pattern: font_data() -> << ... >>.

    case re:run(Content,
                <<"font_data\\(\\)\\s*->\\s*<<\\s*(.+?)\\s*>>\\.">>,
                [dotall, {capture, all_but_first, binary}]) of
        {match, [BinaryContent]} ->
            parse_binary_literal(BinaryContent);
        nomatch ->
            io:format(standard_error, "Error: Could not find font_data() binary in ~s~n", [Path]),
            halt(1)
    end.

parse_binary_literal(BinaryContent) ->
    %% Parse Erlang binary literal like: 16#XX,16#YY,...
    %% Remove comments
    Cleaned = re:replace(BinaryContent, <<"%[^\n]*">>, <<>>, [global, {return, binary}]),

    %% Extract hex values
    case re:run(Cleaned, <<"16#([0-9A-Fa-f]{2})">>, [global, {capture, all_but_first, binary}]) of
        {match, Matches} ->
            Bytes = [binary_to_integer(Hex, 16) || [Hex] <- Matches],
            io:format("Extracted ~w bytes from module~n", [length(Bytes)]),
            list_to_binary(Bytes);
        nomatch ->
            io:format(standard_error, "Error: Could not parse binary literal~n", []),
            halt(1)
    end.

%% ============================================================================
%% Preview
%% ============================================================================

maybe_preview(GlyphData, true) ->
    io:format("~n=== ASCII Art Preview ===~n~n"),
    preview_glyphs(GlyphData);
maybe_preview(_, false) ->
    ok.

preview_glyphs(GlyphData) ->
    lists:foreach(
        fun(CharCode) ->
            Char = [CharCode],
            Offset = (CharCode - 32) * 16,
            GlyphBytes = binary:part(GlyphData, Offset, 16),

            io:format("~3w '~s' ~n", [CharCode, Char]),
            preview_glyph(binary_to_list(GlyphBytes)),
            io:format("~n")
        end,
        lists:seq(32, 126)
    ).

preview_glyph(GlyphBytes) ->
    lists:foreach(
        fun(Byte) ->
            Line = [case Byte band (16#80 bsr Bit) of
                        0 -> $\s;
                        _ -> $#
                    end || Bit <- lists:seq(0, 7)],
            io:format("  ~s~n", [Line])
        end,
        GlyphBytes
    ).

%% ============================================================================
%% Validation
%% ============================================================================

maybe_validate(GlyphData, validate) ->
    validate_glyph_data(GlyphData),
    io:format("✓ Validation passed~n");
maybe_validate(_, _) ->
    ok.

validate_glyph_data(GlyphData) ->
    %% Check size
    ExpectedSize = 95 * 16,
    ActualSize = byte_size(GlyphData),

    case ActualSize of
        ExpectedSize ->
            io:format("✓ Size correct: ~w bytes (95 chars × 16 bytes)~n", [ActualSize]);
        _ ->
            io:format(standard_error, "✗ Size mismatch: expected ~w, got ~w~n",
                     [ExpectedSize, ActualSize]),
            halt(1)
    end,

    %% Check that space (char 32) is all zeros
    SpaceGlyph = binary:part(GlyphData, 0, 16),
    case binary:decode_unsigned(SpaceGlyph) of
        0 ->
            io:format("✓ Space character is blank~n");
        _ ->
            io:format("Warning: Space character has non-zero pixels~n")
    end.

%% ============================================================================
%% Code Generation
%% ============================================================================

maybe_generate(GlyphData, #{output := Output}, generate) ->
    generate_erlang_module(GlyphData, Output);
maybe_generate(GlyphData, #{input := Input}, generate) ->
    %% Auto-generate output filename from input
    BaseName = filename:basename(Input, filename:extension(Input)),
    ModuleName = "vbeam_font_" ++ string:lowercase(BaseName),
    Output = ModuleName ++ ".erl",
    generate_erlang_module(GlyphData, Output);
maybe_generate(_, _, _) ->
    ok.

generate_erlang_module(GlyphData, OutputPath) ->
    %% Extract module name from output path
    BaseName = filename:basename(OutputPath, ".erl"),

    %% Format binary data with comments
    BinaryLines = format_binary_data(GlyphData),

    %% Generate module content
    Content = io_lib:format(
        "%% Generated by render_font_bitmap.escript~n"
        "%% Format: 8x16 bitmap, 95 ASCII characters (32-126)~n"
        "%% Total: 1520 bytes (95 chars × 16 bytes/char)~n"
        "~n"
        "-module(~s).~n"
        "-export([glyph/1, font_binary/0, char_width/0, char_height/0]).~n"
        "~n"
        "%% Get the bitmap for a single character (16 bytes = 8x16 pixels)~n"
        "-spec glyph(char()) -> binary().~n"
        "glyph(Char) when Char >= 32, Char =< 126 ->~n"
        "    Offset = (Char - 32) * 16,~n"
        "    binary:part(font_data(), Offset, 16);~n"
        "glyph(_) ->~n"
        "    glyph($?).  %% Fallback to '?' for unknown chars~n"
        "~n"
        "%% Get the complete font binary~n"
        "-spec font_binary() -> binary().~n"
        "font_binary() -> font_data().~n"
        "~n"
        "%% Font dimensions~n"
        "-spec char_width() -> pos_integer().~n"
        "char_width() -> 8.~n"
        "~n"
        "-spec char_height() -> pos_integer().~n"
        "char_height() -> 16.~n"
        "~n"
        "%% Internal: the bitmap data~n"
        "-spec font_data() -> binary().~n"
        "font_data() ->~n"
        "    <<~n"
        "~s"
        "    >>.~n",
        [BaseName, BinaryLines]
    ),

    %% Write to file
    ok = filelib:ensure_dir(OutputPath),
    ok = file:write_file(OutputPath, Content),

    io:format("✓ Generated ~s~n", [OutputPath]),
    io:format("  Module: ~s~n", [BaseName]),
    io:format("  Characters: 95 (ASCII 32-126)~n"),
    io:format("  Size: 8x16 pixels per glyph~n"),
    io:format("  Total: ~w bytes~n", [byte_size(GlyphData)]).

format_binary_data(GlyphData) ->
    ByteList = binary_to_list(GlyphData),
    format_chars(ByteList, 32, []).

format_chars([], _, Acc) ->
    lists:flatten(lists:reverse(Acc));
format_chars(Bytes, CharCode, Acc) ->
    %% Take 16 bytes for this character
    {GlyphBytes, Rest} = lists:split(16, Bytes),

    %% Format comment
    CharRepr = case CharCode of
        32 -> "Space";
        _ -> [CharCode]
    end,
    Comment = io_lib:format("    %% ~3w - ~s~n", [CharCode, CharRepr]),

    %% Format hex bytes
    HexStr = string:join([io_lib:format("16#~2.16.0B", [B]) || B <- GlyphBytes], ","),

    %% Add trailing comma except for last glyph
    Line = case CharCode of
        126 ->
            io_lib:format("    ~s~n", [HexStr]);
        _ ->
            io_lib:format("    ~s,~n", [HexStr])
    end,

    format_chars(Rest, CharCode + 1, [Line, Comment | Acc]).
