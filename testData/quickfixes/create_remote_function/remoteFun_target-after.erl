-module(remoteFun_target).
-export([bar/0, lll/2]).

bar() -> ok.

lll(_Arg0, _Arg1) ->
  erlang:error(not_implemented).
