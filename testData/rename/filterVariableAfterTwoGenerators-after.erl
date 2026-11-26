-module(filterVariableAfterTwoGenerators).
-export([f/2]).

f(L, M) ->
  [Zz || X <- L, Zz <- M, Zz<caret> > 0].
