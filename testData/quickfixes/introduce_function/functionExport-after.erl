-module(foo).

-export([lll/6]).

foo() -> ok.

lll(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5) ->
  error(not_implemented).