-module(fun_expr_delete).

foo() ->
  fun
    () -> ok;
    Bar<caret>() -> ok
  end.