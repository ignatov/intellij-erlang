-module(varShadowedInGenerator).
-export([f/1]).

f(L) ->
  X = 1,
  {X, [Z || Z <- L, Z<caret> > 0]}.
