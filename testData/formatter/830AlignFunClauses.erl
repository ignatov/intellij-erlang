-module('830AlignFunClauses').

foo(X) ->
  G = fun(Y) -> Y end,
  G(X).
