-module(fun_expr_rename).

foo() ->
  fun
    Foo() -> ok;
    Bar<caret>() -> ok
  end.