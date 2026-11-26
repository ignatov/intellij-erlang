-module(dbg_smoke).
-export([main/0]).

main() ->
    X = 10,
    Y = outer(X),
    io:format("result ~p~n", [Y]).

outer(A) ->
    B = A * 2,
    inner(B) + 1.

inner(C) ->
    D = C + 5,
    D.
