-module(test_bif_tail).
-export([main/0]).

%% Test call_ext_only (tail call to BIF) - should return correct value, not exit early
main() ->
    helper(5).

helper(X) ->
    %% This will compile to call_ext_only for erlang:'+'/2
    erlang:'+'(X, 10).
