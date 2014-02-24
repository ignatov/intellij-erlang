-module(fun_expr_insert).

foo() ->
  fun
    Foo() -> ok;
    Foo() -> ok
  end.