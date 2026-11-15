-module(argumentNames_target).
-export([bar/0, f/9]).

bar() -> ok.

f(X, _Arg1, _Arg2, _Arg3, N, _Arg5, _Arg6, _Arg7, Str) ->
  erlang:error(not_implemented).
