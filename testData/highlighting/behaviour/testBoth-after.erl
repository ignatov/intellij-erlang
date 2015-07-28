-module(testBoth).

-behaviour(b1).

-export([init0/0, init1/1]).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).
