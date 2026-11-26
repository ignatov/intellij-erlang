-module('RepeatedGeneratorPatterns').
-export([p1/1, p2/2, p3/2, p8/1]).
p1(L) ->
    [X || X <- L, X <- X].
p2(L, M) ->
    [X || <warning descr="Unused variable 'X'">X</warning> <- L, X <- M].
p3(L, M) ->
    [X || <warning descr="Unused variable 'X'">X</warning> <- L, X <- M, X > 0].
p8(L) ->
    [Y || X <- L, Y <- [X || X <- X]].
