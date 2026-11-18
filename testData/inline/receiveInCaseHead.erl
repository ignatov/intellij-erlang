foo() ->
  case receive _ -> <caret>T = bar(), T end of
    _ -> ok
  end.

bar() -> 1.
