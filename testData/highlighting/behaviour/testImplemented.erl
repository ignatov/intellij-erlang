-module(testImplemented).

-behaviour(b1<caret>).

init1(_) ->
  erlang:error(not_implemented).

init2(_) ->
  erlang:error(not_implemented).
