foo() ->
  X = 1,
  case X = bar() of
    _ -> <caret>X
  end.

bar() -> 1.
