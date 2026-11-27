-module(compFilterBinding).
-export([f/1]).
f(L) ->
    [{New, W} || K <- L, (<caret>New = K) =/= 1, (W = New) > 0].
