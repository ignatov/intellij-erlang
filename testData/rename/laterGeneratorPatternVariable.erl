-module(laterGeneratorPatternVariable).
-export([f/2]).

f(L, M) ->
  [X || X <- L, X<caret> <- X ++ M].
