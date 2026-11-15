-module(freshVarsInReceiveClauses).
-export([f/0]).

f() ->
  receive
    {a, X<caret>} -> X;
    {b, X} -> X
  end.
