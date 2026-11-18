-module(varBoundInReceiveInCaseClause).
-export([f/1]).

f(T) ->
  case T of wait -> receive {reply, R<caret>} -> ok end; _ -> R = none end,
  R.
