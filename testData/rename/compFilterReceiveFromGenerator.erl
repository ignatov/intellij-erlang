-module(compFilterReceiveFromGenerator).
-export([f/2]).
f(X, L) ->
    [Y || <caret>Y <- L, receive Y -> true after X -> false end].
