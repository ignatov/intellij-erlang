-module(varInReceivePatternsUsedInBody).
-export([f/0]).

f() ->
  Child = self(),
  receive
    {x, Child} -> C<caret>hild;
    {y, Child} -> ok
  end.
