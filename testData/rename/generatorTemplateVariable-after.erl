-module(generatorTemplateVariable).
-export([f/2]).

f(L, M) ->
  [Zz<caret> || X <- L, Zz <- X ++ M].
