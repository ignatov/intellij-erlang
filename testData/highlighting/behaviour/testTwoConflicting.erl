-module<caret>(testTwoConflicting).
-behaviour(b1).
-behaviour(b3).

-export([init0/0, init1/1, init3/1]).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).

init3(_) ->
  erlang:error(not_implemented).
