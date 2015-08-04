-module(testUndefinedBehaviour).
-behaviour(b1).
-behaviour(b_info).
-behaviour(<warning>m1</warning>).
-behaviour(<warning>notExists</warning>).

-export([init0/0, init1/1]).

init0() ->
  erlang:error(not_implemented).

init1(_) ->
  erlang:error(not_implemented).
