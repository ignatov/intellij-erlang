-module(compFilterReceiveFromGenerator).
-export([f/2]).
f(X, L) ->
    [Zz || <caret>Zz <- L, receive Zz -> true after X -> false end].
