-export([f/1]).

f(P) ->
  max(1, 2),
  <warning descr="Unresolved function link/2">link</warning>(P, []),
  <warning descr="Unresolved function exit_signal/2">exit_signal</warning>(P, x).
