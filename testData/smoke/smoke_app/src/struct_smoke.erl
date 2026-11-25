-module(struct_smoke).
-export([alpha/1, beta/0, gamma/2]).

-record(state, {a, b}).
-define(LIMIT, 10).

alpha(0) -> zero;
alpha(N) when N > 0 -> positive;
alpha(_) -> negative.

beta() ->
    #state{a = 1, b = ?LIMIT}.

gamma(X, Y) ->
    X + Y.
