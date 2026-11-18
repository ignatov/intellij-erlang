-export([test/0, nested/0, other_call/0]).
-compile({parse_transform, ms_transform}).

test() ->
  dbg:fun2ms(fun(X) -> {X, object(), bindings()} end).

nested() ->
  ets:fun2ms(fun(X) -> fun() -> {X, <warning descr="Unresolved function object/0">object</warning>()} end end).

other_call() ->
  lists:map(fun(X) -> {X, <warning descr="Unresolved function object/0">object</warning>()} end, []).
