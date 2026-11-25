-module(laterGeneratorPatternVariable).
-export([f/2]).

f(L, M) ->
  [Zz || X <- L, Zz<caret> <- X ++ M].
