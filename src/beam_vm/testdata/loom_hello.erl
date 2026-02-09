-module(loom_hello).
-export([main/0, greet/0]).

%% Simplest possible: just returns an atom
main() ->
    greet().

greet() ->
    'Hello from V-on-BEAM on Loom!'.
