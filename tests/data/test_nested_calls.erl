-module(test_nested_calls).
-export([main/0, outer/1, inner/1]).

%% Test nested function calls to verify call_ext_only and return fixes
main() ->
    outer(10).

outer(X) ->
    Y = inner(X),
    Y + 5.

inner(X) ->
    %% Tail call to BIF
    erlang:'+'(X, 2).
