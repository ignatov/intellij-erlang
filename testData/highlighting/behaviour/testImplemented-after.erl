-module(testImplemented).

-behaviour(b1).

-export([init1/1, init2/1]).

init1(_) ->
  erlang:error(not_implemented).

init2(_) ->
  erlang:error(not_implemented).
