-module(loom_hello_print).
-export([main/0]).

%% This calls io:format which the interpreter maps to its output callback
main() ->
    io:format("Hello from V-on-BEAM on Loom!~n"),
    ok.
