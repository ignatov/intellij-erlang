-module(varBoundInReceiveTimeout).
-export([f/0]).

f() ->
  receive after (Zz = 100) -> ok end,
  Zz<caret>.
