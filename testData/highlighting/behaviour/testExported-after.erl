-module(testExported).

-behaviour(b1).
-behaviour(b2).

-export([init1/1, init2/1]).

init1(_) ->
  erlang:error(not_implemented).

init2(_) ->
  erlang:error(not_implemented).
