%% agree: 5
%% unused_var Y, unused_function f2/0, unused_type t1, unbound_var Unbound1, undefined_function missing_1/2
-module(pbt).
-export([f1/1]).
-type t1() :: integer().
f1(X) ->
    Y = 1,
    missing_1(X, Unbound1).
f2() ->
    ok.
