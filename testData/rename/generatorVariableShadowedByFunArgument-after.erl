-module(generatorVariableShadowedByFunArgument).
-export([f/1]).

f(L) ->
  [{G, Zz} || <caret>Zz <- L, G <- [fun(Y) -> Y end]].
