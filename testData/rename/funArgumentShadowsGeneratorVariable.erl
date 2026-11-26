-module(funArgumentShadowsGeneratorVariable).
-export([f/1]).

f(L) ->
  [G || Y <- L, G <- [fun(Y) -> <caret>Y end]].
