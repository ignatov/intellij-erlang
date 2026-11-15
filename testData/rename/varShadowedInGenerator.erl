-module(varShadowedInGenerator).
-export([f/1]).

f(L) ->
  X = 1,
  {X, [X || X <- L, X<caret> > 0]}.
