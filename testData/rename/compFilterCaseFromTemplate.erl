-module(compFilterCaseFromTemplate).
-export([f/2]).
f(X, L) ->
    [<caret>Y || Y <- L, case X of Y -> true; _ -> false end].
