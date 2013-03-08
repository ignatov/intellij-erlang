-module(test).

-behaviour(b1).
-behaviour(b2).

init1(_) ->
  erlang:error(not_implemented).

init2(_) ->
  erlang:error(not_implemented).
