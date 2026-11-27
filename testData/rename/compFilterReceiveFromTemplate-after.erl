-module(compFilterReceiveFromTemplate).
-export([f/2]).
f(X, L) ->
    [<caret>Zz || Zz <- L, receive Zz -> true after X -> false end].
