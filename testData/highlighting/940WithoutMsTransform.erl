-export([test/0]).

test() ->
  ets:fun2ms(fun(X) -> {X, element(3, <warning descr="Unresolved function object/0">object</warning>())} end).
