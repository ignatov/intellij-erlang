-module(varUsedInReceiveTimeoutInCaseClause).
-export([f/1]).

f(T) ->
  case T of a -> Zz = 1, receive after Zz -> ok end; b -> Zz<caret> = 2 end,
  Zz.
