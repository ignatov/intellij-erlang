-module(varBoundInReceiveAfterInCaseClause).
-export([f/1]).

f(T) ->
  case T of a -> receive after 1 -> X<caret> = 1 end; b -> X = 2 end,
  X.
