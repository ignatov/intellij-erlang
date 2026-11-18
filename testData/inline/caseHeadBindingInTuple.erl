foo() ->
  case {<caret>X = bar(), 1} of
    _ -> X
  end.

bar() -> 1.
