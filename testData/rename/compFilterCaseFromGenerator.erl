-module(compFilterCaseFromGenerator).
-export([f/2]).
f(X, L) ->
    [Y || <caret>Y <- L, case X of Y -> true; _ -> false end].
