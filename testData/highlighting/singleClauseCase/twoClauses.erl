-module(twoClauses).
-export([f/1]).

f(X) ->
  case X of
    {ok, Y} -> Y;
    _ -> error
  end.
