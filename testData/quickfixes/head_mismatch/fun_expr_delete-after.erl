-module(fun_expr_delete).

foo() ->
  fun
    () -> ok;
    () -> ok
  end.