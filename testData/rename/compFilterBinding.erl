-module(compFilterBinding).
-export([f/1]).
f(L) ->
    [{V, W} || K <- L, (<caret>V = K) =/= 1, (W = V) > 0].
