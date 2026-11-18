-module(varBoundInReceiveAfterInCaseClause).
-export([f/1]).

f(T) ->
  case T of a -> receive after 1 -> Zz<caret> = 1 end; b -> Zz = 2 end,
  Zz.
