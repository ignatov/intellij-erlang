-module(funReferenceInsideFun).
-export([f/1]).

f(L) ->
  F = fun(X) ->
    lists:map(fun erlang:abs/1, X)
  end,
  F(L).
