foo() ->
  case try bar() catch _:_ -> error end of
    _ -> ok
  end.

bar() -> 1.
