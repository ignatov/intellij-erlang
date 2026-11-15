-module(varShadowedByNamedFunFromOuter).
-export([f/0]).

f() ->
  F<caret> = 1,
  G = fun F(0) -> F; F(N) -> F(N - 1) end,
  {F, G}.
