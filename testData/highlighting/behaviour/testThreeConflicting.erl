-module<caret>(testThreeConflicting).
-behaviour(b1).
-behaviour(b2).
-behaviour(b3).

-export([init0/0, init1/1, init3/1, init4/1]).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).

init3(_) ->
  erlang:error(not_implemented).

init4(_) ->
  erlang:error(not_implemented).
