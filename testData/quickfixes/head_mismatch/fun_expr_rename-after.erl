-module(fun_expr_rename).

foo() ->
  fun
    Foo() -> ok;
    Foo() -> ok
  end.