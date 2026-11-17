-module(fixAll_target).
-export([bar/0, f/1, g/1]).

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).

g(N) ->
  erlang:error(not_implemented).
