-module(pinfo_smoke).
-export([f/0, add/2]).
-define(ADD(A, B), A + B).

%% Adds two integers.
-spec add(integer(), integer()) -> integer().
add(X, Y) -> X + Y.

f() ->
    L = lists:map(fun(E) -> E end, [1]),
    M = ?ADD(1, 2),
    add(L, M).
