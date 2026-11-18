-module(varBoundInReceiveClauseAndAfter).
-export([f/0]).

f() ->
  receive {a, X<caret>} -> ok after 10 -> X = 1 end,
  X.
