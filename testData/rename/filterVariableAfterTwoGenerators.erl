-module(filterVariableAfterTwoGenerators).
-export([f/2]).

f(L, M) ->
  [X || X <- L, X <- M, X<caret> > 0].
