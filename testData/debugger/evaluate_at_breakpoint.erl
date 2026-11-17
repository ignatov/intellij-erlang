-module(evaluate_at_breakpoint).
-export([f/1]).

f(A) ->
  B = A + 1,
  B.
