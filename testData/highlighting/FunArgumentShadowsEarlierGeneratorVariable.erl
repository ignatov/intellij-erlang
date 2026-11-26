-export([f/1, g/1, h/1]).

f(L) ->
  [G || <warning descr="Unused variable 'Y'">Y</warning> <- L, G <- [fun(Y) -> Y end]].

g(L) ->
  [G || <warning descr="Unused variable 'Y'">Y</warning> <- L, G <- L, fun(Y) -> Y end(G)].

h(L) ->
  [G || Y <- L, G <- [fun(Z) -> Y + Z end]].
