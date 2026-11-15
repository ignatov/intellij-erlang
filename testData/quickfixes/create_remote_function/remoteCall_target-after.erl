-module(remoteCall_target).
-export([bar/0, lll/3]).

bar() -> ok.

lll(N, Aaaa, OOO) ->
  erlang:error(not_implemented).
