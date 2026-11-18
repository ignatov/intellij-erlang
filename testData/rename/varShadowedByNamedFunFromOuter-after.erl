-module(varShadowedByNamedFunFromOuter).
-export([f/0]).

f() ->
  Zz<caret> = 1,
  G = fun F(0) -> F; F(N) -> F(N - 1) end,
  {Zz, G}.
