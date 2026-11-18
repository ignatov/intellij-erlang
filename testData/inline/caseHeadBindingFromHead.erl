foo() ->
  case <caret>X = bar() of
    _ -> X
  end.

bar() -> 1.
