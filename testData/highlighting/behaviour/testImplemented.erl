-module(testImplemented).

-behaviour(b1<caret>).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).
