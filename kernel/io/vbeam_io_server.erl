%%% @doc Minimal I/O server for bare-metal serial and framebuffer output.
%%%
%%% Implements Erlang's I/O protocol to handle io:format/2 and friends.
%%% Routes output to serial (COM1), framebuffer, and in-memory log buffer.
%%%
%%% NO OTP dependencies - pure Erlang for bare metal.
%%% Uses gen_server in OTP mode, simplified message loop for bare metal.
%%%
%%% @end
-module(vbeam_io_server).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    set_group_leader/0,
    write/1,
    writeln/1,
    get_log/0
]).

%% gen_server callbacks (OTP mode)
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(CHAR_WIDTH, 8).
-define(CHAR_HEIGHT, 16).
-define(TAB_WIDTH, 8).
-define(MAX_LOG_SIZE, 1000).

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

-type fb_info() :: #{
    base => non_neg_integer(),
    width => pos_integer(),
    height => pos_integer(),
    stride => pos_integer()
}.

-type state() :: #{
    serial => boolean(),
    framebuffer => false | fb_info(),
    log => boolean(),
    log_buffer => [binary()],  % Ring buffer of log entries
    log_count => non_neg_integer(),
    max_log_size => pos_integer(),
    cursor_x => non_neg_integer(),
    cursor_y => non_neg_integer(),
    margin => non_neg_integer()
}.

%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------

%% @doc Start the I/O server with configuration options.
%% Options:
%%   serial :: boolean() - Enable serial output (default: true)
%%   framebuffer :: false | fb_info() - Framebuffer configuration
%%   log :: boolean() - Enable in-memory logging (default: true)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Set this I/O server as the group leader for the calling process.
%% After this, io:format/2 etc. will route through this server.
-spec set_group_leader() -> true | {error, not_started}.
set_group_leader() ->
    %% Get the actual pid of the registered server
    case whereis(?SERVER) of
        undefined ->
            {error, not_started};
        Pid ->
            group_leader(Pid, self())
    end.

%% @doc Direct write (bypasses I/O protocol, for kernel messages).
-spec write(iodata()) -> ok.
write(Data) ->
    gen_server:cast(?SERVER, {direct_write, Data}).

%% @doc Direct write with newline.
-spec writeln(iodata()) -> ok.
writeln(Data) ->
    write([Data, <<"\n">>]).

%% @doc Get accumulated log buffer.
-spec get_log() -> binary().
get_log() ->
    gen_server:call(?SERVER, get_log).

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------

%% @private
init(Opts) ->
    Serial = maps:get(serial, Opts, true),
    Framebuffer = maps:get(framebuffer, Opts, false),
    Log = maps:get(log, Opts, true),
    RawMaxLogSize = maps:get(max_log_size, Opts, ?MAX_LOG_SIZE),

    %% BUG 1 FIX: Validate max_log_size (positive, capped at 100MB)
    MaxLogBytes = 100 * 1024 * 1024,  % BUG 5 FIX: Clamp to 100MB max
    MaxLogSize = case is_integer(RawMaxLogSize) andalso RawMaxLogSize > 0 of
        true -> min(RawMaxLogSize, MaxLogBytes);
        false -> ?MAX_LOG_SIZE  %% Use default if invalid
    end,

    %% Initial cursor position (left margin, top of screen)
    Margin = 8,

    State = #{
        serial => Serial,
        framebuffer => Framebuffer,
        log => Log,
        log_buffer => [],
        log_count => 0,
        max_log_size => MaxLogSize,
        cursor_x => Margin,
        cursor_y => 20,
        margin => Margin
    },

    {ok, State}.

%% @private
handle_call(get_log, _From, #{log_buffer := LogBuf} = State) ->
    %% BUG 2 FIX: Cap returned log data to 1MB max
    MaxBytes = 1024 * 1024,
    CombinedLog = truncate_log_entries(lists:reverse(LogBuf), MaxBytes),
    {reply, CombinedLog, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({direct_write, Data}, State) ->
    String = unicode:characters_to_binary(Data),
    NewState = output_all(String, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Handle Erlang I/O protocol requests
handle_info({io_request, From, ReplyAs, Request}, State) ->
    case handle_io_request(Request, State) of
        {ok, Reply, NewState} ->
            From ! {io_reply, ReplyAs, Reply},
            {noreply, NewState};
        {error, Reason, NewState} ->
            From ! {io_reply, ReplyAs, {error, Reason}},
            {noreply, NewState}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------------------------
%%% I/O Protocol Handling
%%% ----------------------------------------------------------------------------

%% @doc Handle individual I/O protocol requests
-spec handle_io_request(term(), state()) -> {ok, term(), state()} | {error, term(), state()}.

%% put_chars with explicit encoding and data
handle_io_request({put_chars, Encoding, Chars}, State) ->
    try
        String = encode_chars(Encoding, Chars),
        NewState = output_all(String, State),
        {ok, ok, NewState}
    catch
        _:Reason ->
            {error, Reason, State}
    end;

%% put_chars with encoding and MFA
handle_io_request({put_chars, Encoding, Mod, Fun, Args}, State) ->
    %% BUG 6 FIX: Validate arity and module whitelist, enforce size limit
    %% HIGH FIX: Check is_list(Args) before calling length to prevent crash
    case is_list(Args) of
        false ->
            {error, {invalid_args, Args}, State};
        true ->
            Arity = length(Args),
            case is_safe_mfa(Mod, Fun, Arity) of
                true when Arity >= 0, Arity =< 3 ->
                    try
                        Chars = apply(Mod, Fun, Args),
                        String = encode_chars(Encoding, Chars),
                        %% BUG 6 FIX: Truncate output to 64KB max
                        MaxOutputSize = 64 * 1024,
                        TruncatedString = case byte_size(String) of
                            Size when Size > MaxOutputSize ->
                                <<Prefix:MaxOutputSize/binary, _/binary>> = String,
                                Prefix;
                            _ ->
                                String
                        end,
                        NewState = output_all(TruncatedString, State),
                        {ok, ok, NewState}
                    catch
                        _:Reason ->
                            {error, Reason, State}
                    end;
                _ ->
                    {error, {unauthorized_mfa, Mod, Fun}, State}
            end
    end;

%% Legacy put_chars (assume latin1)
handle_io_request({put_chars, Chars}, State) ->
    handle_io_request({put_chars, latin1, Chars}, State);

%% Unknown request
handle_io_request(_Request, State) ->
    {error, {error, request}, State}.

%% @doc Encode characters based on encoding
-spec encode_chars(unicode | latin1, iodata()) -> binary().

%% BUG 6 FIX: Whitelist for safe MFA calls with module restriction
%% TIGHTENED: Only allow specific safe formatting functions, NO wildcard module access
is_safe_mfa(erlang, Fun, 1) when Fun =:= integer_to_list;
                                  Fun =:= float_to_list;
                                  Fun =:= atom_to_list;
                                  Fun =:= list_to_binary ->
    true;
is_safe_mfa(io_lib, format, 2) -> true;
is_safe_mfa(io_lib, write, 1) -> true;
is_safe_mfa(_Mod, _Fun, _Arity) ->
    false.

encode_chars(unicode, Chars) ->
    unicode:characters_to_binary(Chars);
encode_chars(latin1, Chars) ->
    %% Convert Latin-1 iodata to binary
    iolist_to_binary(Chars).

%%% ----------------------------------------------------------------------------
%%% Output Backends
%%% ----------------------------------------------------------------------------

%% @doc Route output to all enabled backends
-spec output_all(binary(), state()) -> state().
output_all(String, State) ->
    #{serial := Serial,
      framebuffer := Fb,
      log := Log} = State,

    %% Output to serial
    State1 = case Serial of
        true -> output_serial(String), State;
        false -> State
    end,

    %% Output to framebuffer
    State2 = case Fb of
        false -> State1;
        FbInfo -> output_framebuffer(String, State1#{framebuffer := FbInfo})
    end,

    %% Output to log
    case Log of
        true -> output_log(String, State2);
        false -> State2
    end.

%% @doc Write string to serial (COM1 @ 0x3F8).
%% In OTP: write to stdout for testing.
%% On bare metal: port I/O to COM1.
-spec output_serial(binary()) -> ok.
output_serial(String) ->
    %% OTP mode: write to stdout
    %% Bare metal mode: would use port I/O (out instruction)
    io:put_chars(standard_io, String),
    ok.

%% @doc Write string to framebuffer at current cursor position.
%% In OTP: log to stderr for testing.
%% On bare metal: use bitmap font to render characters.
-spec output_framebuffer(binary(), state()) -> state().
output_framebuffer(String, State) ->
    #{framebuffer := FbInfo,
      cursor_x := X,
      cursor_y := Y,
      margin := Margin} = State,

    #{width := Width,
      height := Height} = FbInfo,

    %% Process each character, updating cursor position
    {NewX, NewY} = process_string(String, X, Y, Margin, Width, Height),

    %% In OTP mode, just log to stderr
    io:put_chars(standard_error, ["[FB] ", String]),

    State#{cursor_x := NewX, cursor_y := NewY}.

%% @doc Process string character by character, tracking cursor
-spec process_string(binary(), non_neg_integer(), non_neg_integer(),
                    non_neg_integer(), pos_integer(), pos_integer()) ->
                    {non_neg_integer(), non_neg_integer()}.
process_string(<<>>, X, Y, _Margin, _Width, _Height) ->
    {X, Y};
process_string(<<Char:8, Rest/binary>>, X, Y, Margin, Width, Height) ->
    {NewX, NewY} = advance_cursor(Char, X, Y, Margin, Width, Height),
    process_string(Rest, NewX, NewY, Margin, Width, Height).

%% @doc Advance cursor based on character
-spec advance_cursor(byte(), non_neg_integer(), non_neg_integer(),
                    non_neg_integer(), pos_integer(), pos_integer()) ->
                    {non_neg_integer(), non_neg_integer()}.
advance_cursor($\n, _X, Y, Margin, _Width, Height) ->
    %% Newline: return to margin, advance Y
    NewY = Y + ?CHAR_HEIGHT,
    %% Check for scroll (simplified: wrap for now)
    FinalY = case NewY >= Height of
        true -> Height - ?CHAR_HEIGHT;
        false -> NewY
    end,
    {Margin, FinalY};
advance_cursor($\r, _X, Y, Margin, _Width, _Height) ->
    %% Carriage return: return to margin
    {Margin, Y};
advance_cursor($\t, X, Y, Margin, Width, _Height) ->
    %% Tab: advance to next 8-character boundary
    TabStop = ((X - Margin) div (?CHAR_WIDTH * ?TAB_WIDTH) + 1) * (?CHAR_WIDTH * ?TAB_WIDTH) + Margin,
    NewX = case TabStop >= Width of
        true -> Margin; %% Wrap to next line would go here
        false -> TabStop
    end,
    {NewX, Y};
advance_cursor(_Char, X, Y, Margin, Width, Height) ->
    %% Regular character: advance X by character width
    NewX = X + ?CHAR_WIDTH,
    case NewX >= Width of
        true ->
            %% Wrap to next line
            NewY = Y + ?CHAR_HEIGHT,
            %% Check for scroll (simplified: wrap for now)
            FinalY = case NewY >= Height of
                true -> Height - ?CHAR_HEIGHT;
                false -> NewY
            end,
            {Margin, FinalY};
        false ->
            {NewX, Y}
    end.

%% @doc Append to in-memory log buffer (ring buffer with max size)
-spec output_log(binary(), state()) -> state().
output_log(String, #{log_buffer := LogBuf, log_count := Count, max_log_size := MaxSize} = State) ->
    %% BUG 3 FIX: Truncate individual log entries to max 64KB
    MaxEntrySize = 64 * 1024,
    TruncatedString = case byte_size(String) of
        Size when Size > MaxEntrySize ->
            <<Prefix:MaxEntrySize/binary, _/binary>> = String,
            Prefix;
        _ ->
            String
    end,

    NewLogBuf = case Count >= MaxSize of
        true ->
            %% Drop oldest entry (at tail of list)
            [TruncatedString | lists:droplast(LogBuf)];
        false ->
            [TruncatedString | LogBuf]
    end,
    NewCount = min(Count + 1, MaxSize),
    State#{log_buffer := NewLogBuf, log_count := NewCount}.

%% BUG 2 FIX: Helper to truncate log entries to fit within byte limit
-spec truncate_log_entries([binary()], non_neg_integer()) -> binary().
truncate_log_entries(Entries, MaxBytes) ->
    truncate_log_entries(Entries, MaxBytes, <<>>).

truncate_log_entries([], _MaxBytes, Acc) ->
    Acc;
truncate_log_entries([Entry | Rest], MaxBytes, Acc) ->
    AccSize = byte_size(Acc),
    EntrySize = byte_size(Entry),
    case AccSize + EntrySize of
        Total when Total =< MaxBytes ->
            truncate_log_entries(Rest, MaxBytes, <<Acc/binary, Entry/binary>>);
        _ ->
            %% Would exceed limit, stop here
            Acc
    end.
