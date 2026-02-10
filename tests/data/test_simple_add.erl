-module(test_simple_add).
-export([main/0]).

%% Ultra-simple test - just call a BIF and return
main() ->
    erlang:'+'(5, 10).
