foo() ->
  case X = bar() of
    _ -> <caret>X
  end.

bar() -> 1.
