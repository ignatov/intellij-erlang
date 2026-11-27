-module(compFilterCaseFromTemplate).
-export([f/2]).
f(X, L) ->
    [<caret>Zz || Zz <- L, case X of Zz -> true; _ -> false end].
