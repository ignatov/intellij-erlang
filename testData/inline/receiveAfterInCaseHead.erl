foo() ->
  case receive after 0 -> <caret>T = bar(), T end of
    _ -> ok
  end.

bar() -> 1.
