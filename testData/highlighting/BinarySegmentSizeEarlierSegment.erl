-export([function_head/1, fun_head/1, case_pattern/1, generator/1, body_match/1, spaced/2, expression/2,
         generator_shadow/2, fun_shadow/2, later_segment/1, body_from_size/1]).

function_head(<<L:8, X:L>>) -> X.

fun_head(B) -> fun(<<M:8, X:M>>) -> X end(B).

case_pattern(T) ->
  case T of
    <<N:8, X:N>> -> X
  end.

generator(L) ->
  [X || <<N:8, X:N>> <- L].

body_match(B) ->
  <<N:8, X:N>> = B, X.

spaced(V, B) ->
  F = fun(<<X : V>>) -> X end,
  F(B).

expression(N, B) ->
  <<X:(N * 8)>> = B, X.

generator_shadow(N, L) ->
  [X || {<warning descr="Unused variable 'N'">N</warning>, <<X:N>>} <- L].

fun_shadow(N, B) ->
  F = fun(<warning descr="Unused variable 'N'">N</warning>, <<X:N>>) -> X end,
  F(N, B).

later_segment(<<X:<error descr="Variable 'N' is unbound">N</error>, <warning descr="Unused variable 'N'">N</warning>:8>>) -> X.

body_from_size(<<_:<error descr="Variable 'N' is unbound">N</error>>>) -> <error descr="Variable 'N' is unbound">N</error>.
