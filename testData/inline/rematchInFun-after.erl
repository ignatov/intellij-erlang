foo() ->
  F = fun() -> bar() = baz(), X end,
  F().
