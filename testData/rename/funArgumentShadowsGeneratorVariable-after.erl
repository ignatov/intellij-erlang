-module(funArgumentShadowsGeneratorVariable).
-export([f/1]).

f(L) ->
  [G || Y <- L, G <- [fun(Zz) -> <caret>Zz end]].
