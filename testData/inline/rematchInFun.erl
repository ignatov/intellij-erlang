foo() ->
  <caret>X = bar(),
  F = fun() -> X = baz(), X end,
  F().
