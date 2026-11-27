-module(compFilterReceiveFromTemplate).
-export([f/2]).
f(X, L) ->
    [<caret>Y || Y <- L, receive Y -> true after X -> false end].
