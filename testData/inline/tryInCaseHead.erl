foo() ->
  case try <caret>T = bar(), T catch _:_ -> error end of
    _ -> ok
  end.

bar() -> 1.
