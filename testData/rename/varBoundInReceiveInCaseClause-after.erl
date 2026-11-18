-module(varBoundInReceiveInCaseClause).
-export([f/1]).

f(T) ->
  case T of wait -> receive {reply, Zz<caret>} -> ok end; _ -> Zz = none end,
  Zz.
