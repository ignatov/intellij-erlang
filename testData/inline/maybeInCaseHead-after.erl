foo() ->
  case maybe bar() end of
    _ -> ok
  end.

bar() -> 1.
