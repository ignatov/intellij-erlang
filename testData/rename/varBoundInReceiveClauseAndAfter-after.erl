-module(varBoundInReceiveClauseAndAfter).
-export([f/0]).

f() ->
  receive {a, Zz<caret>} -> ok after 10 -> Zz = 1 end,
  Zz.
