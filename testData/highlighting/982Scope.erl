-export([pattern_repeats_head/0, guard_uses_head/0, clause_scope/0, bound_in_every_clause/0,
  head_use_of_clause_binding/0, head_fun_does_not_leak/0, nested_call_in_head/0]).

pattern_repeats_head() ->
  case X = fun_b() of
    X -> X;
    _ -> X
  end.

guard_uses_head() ->
  case G = fun_b() of
    _ when G =:= b -> ok;
    _ -> error
  end.

clause_scope() ->
  case fun_b() of
    a -> Y = 1, Y;
    _ -> <error descr="Variable 'Y' is unbound">Y</error>
  end.

bound_in_every_clause() ->
  case fun_b() of
    a -> Z = 1;
    _ -> Z = 2
  end,
  Z.

head_use_of_clause_binding() ->
  case fun_c(<error descr="Variable 'W' is unbound">W</error>) of
    _ -> <warning descr="Unused variable 'W'">W</warning> = 1
  end.

head_fun_does_not_leak() ->
  case fun_c(fun(Q) -> Q end) of
    _ -> <error descr="Variable 'Q' is unbound">Q</error>
  end.

nested_call_in_head() ->
  case fun_c(fun_c(S = 1)) of
    _ -> S
  end,
  S.

fun_b() -> b.
fun_c(X) -> X.
