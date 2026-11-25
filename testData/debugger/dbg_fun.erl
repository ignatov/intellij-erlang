-module(dbg_fun).
-export([run/0]).

run() ->
  F = fun(X) ->
    X + 1
  end,
  F(1).
