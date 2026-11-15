-module(varShadowedInGeneratorFromOuter).
-export([f/1]).

f(L) ->
  Zz<caret> = 1,
  [X || {X, _} <- L, X > 0],
  Zz.
