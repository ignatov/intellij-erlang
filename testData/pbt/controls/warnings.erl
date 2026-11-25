-module(pbt).
-export([f1/1]).
-type t1() :: integer().
f1(X) ->
    Y = 1,
    missing_1(X, Unbound1).
f2() ->
    ok.
