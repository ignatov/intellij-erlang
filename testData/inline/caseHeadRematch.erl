foo() ->
  <caret>X = 1,
  case X = bar() of
    _ -> X
  end.

bar() -> 1.
