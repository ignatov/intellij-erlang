-export([f/1]).

f(L) ->
  [G || <warning descr="Unused variable 'Y'">Y</warning> <- L, G <- [fun(Y) -> Y end]].
