-module(varInReceivePatternsUsedInBody).
-export([f/0]).

f() ->
  P<caret>id = self(),
  receive
    {x, Pid} -> Pid;
    {y, Pid} -> ok
  end.
