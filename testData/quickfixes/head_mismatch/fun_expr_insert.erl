-module(fun_expr_insert).

foo() ->
  fun
    Foo() -> ok;
    <caret>() -> ok
  end.