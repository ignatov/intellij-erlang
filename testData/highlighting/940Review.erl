-export([guard/0, guard_bindings/0, fun_ref/0, named_fun/0, parenthesized/0, nested_parenthesized/0]).
-compile({parse_transform, ms_transform}).

guard() ->
  ets:fun2ms(fun(X) when element(1, object()) > 1 -> X end).

guard_bindings() ->
  ets:fun2ms(fun(X) when is_tuple(bindings()) -> X end).

fun_ref() ->
  ets:fun2ms(fun <warning descr="Unresolved function object/0">object</warning>/0).

named_fun() ->
  ets:fun2ms(fun F(X) -> {F, X, <warning descr="Unresolved function object/0">object</warning>()} end).

parenthesized() ->
  ets:fun2ms((fun(X) -> {X, object()} end)).

nested_parenthesized() ->
  ets:fun2ms(((fun(X) -> {X, bindings()} end))).
