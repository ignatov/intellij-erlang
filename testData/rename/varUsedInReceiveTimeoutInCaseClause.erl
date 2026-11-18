-module(varUsedInReceiveTimeoutInCaseClause).
-export([f/1]).

f(T) ->
  case T of a -> X = 1, receive after X -> ok end; b -> X<caret> = 2 end,
  X.
