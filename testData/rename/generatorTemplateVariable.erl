-module(generatorTemplateVariable).
-export([f/2]).

f(L, M) ->
  [X<caret> || X <- L, X <- X ++ M].
