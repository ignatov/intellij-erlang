-export([test/0, test_bindings/0, outside/0]).
-include_lib("stdlib/include/ms_transform.hrl").

test() ->
  ets:fun2ms(fun(X) -> {X, element(3, object())} end).

test_bindings() ->
  ets:fun2ms(fun(X) -> {X, bindings()} end).

outside() -> <warning descr="Unresolved function object/0">object</warning>().
