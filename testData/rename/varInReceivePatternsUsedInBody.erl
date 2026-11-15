-module(varInReceivePatternsUsedInBody).
-export([f/0]).

f() ->
  Pid = self(),
  receive
    {x, Pid} -> P<caret>id;
    {y, Pid} -> ok
  end.
