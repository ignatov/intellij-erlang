-module(varBoundInReceiveTimeout).
-export([f/0]).

f() ->
  receive after (T = 100) -> ok end,
  T<caret>.
