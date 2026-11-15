-export([reported/0, nested_in_call/0, used_after/0, inner_case/1, in_pattern/0, fun_in_head/0]).

fun_b() -> b.
f(X) -> X.

reported() ->
  case VarB = fun_b() of
    a -> a;
    _ -> VarB
  end.

nested_in_call() ->
  case f(Socket = fun_b()) of
    ok -> Socket;
    _ -> {error, Socket}
  end.

used_after() ->
  case Value = fun_b() of
    _ -> ok
  end,
  Value.

inner_case(A) ->
  case A of
    _ -> case Inner = fun_b() of _ -> Inner end
  end.

in_pattern() ->
  case Same = fun_b() of
    Same -> ok
  end.

fun_in_head() ->
  case (fun(Y) -> Y end)(1) of
    _ -> <error descr="Variable 'Y' is unbound">Y</error>
  end.
