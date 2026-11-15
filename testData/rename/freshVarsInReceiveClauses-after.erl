-module(freshVarsInReceiveClauses).
-export([f/0]).

f() ->
  receive
    {a, Y<caret>} -> Y;
    {b, X} -> X
  end.
