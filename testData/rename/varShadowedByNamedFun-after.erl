-module(varShadowedByNamedFun).
-export([f/0]).

f() ->
  F = 1,
  G = fun Zz(0) -> Zz; Zz(N) -> Zz<caret>(N - 1) end,
  {F, G}.
