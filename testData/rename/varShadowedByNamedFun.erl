-module(varShadowedByNamedFun).
-export([f/0]).

f() ->
  F = 1,
  G = fun F(0) -> F; F(N) -> F<caret>(N - 1) end,
  {F, G}.
