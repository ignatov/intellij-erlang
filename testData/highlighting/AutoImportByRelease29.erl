-export([f/1]).

f(P) ->
  max(1, 2),
  link(P, []),
  exit_signal(P, x).
