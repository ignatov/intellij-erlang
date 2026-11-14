-export([head_match/0, head_match_used_after/0, head_match_in_call/1, head_match_unused/0]).

head_match() ->
  case VarB = fun_b() of
    a -> a;
    _ -> VarB
  end.

head_match_used_after() ->
  case VarC = fun_b() of
    _ -> ok
  end,
  VarC.

head_match_in_call(D) ->
  case fun_c(Socket = D) of
    ok -> Socket;
    Other -> Other
  end.

head_match_unused() ->
  case <warning descr="Unused variable 'VarU'">VarU</warning> = fun_b() of
    _ -> ok
  end.

fun_b() -> b.
fun_c(X) -> X.
