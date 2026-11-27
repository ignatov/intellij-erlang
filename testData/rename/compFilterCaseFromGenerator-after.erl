-module(compFilterCaseFromGenerator).
-export([f/2]).
f(X, L) ->
    [Zz || <caret>Zz <- L, case X of Zz -> true; _ -> false end].
