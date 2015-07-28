-module(testUnresolved).

-behaviour(b1).
-behaviour(<warning>unresolved</warning>).

-export([init0/0, init1/1]).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).
