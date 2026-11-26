-module(generatorVariableShadowedByFunArgument).
-export([f/1]).

f(L) ->
  [{G, Y} || <caret>Y <- L, G <- [fun(Y) -> Y end]].
