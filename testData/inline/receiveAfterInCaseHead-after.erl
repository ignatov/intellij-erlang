foo() ->
  case receive after 0 -> bar() end of
    _ -> ok
  end.

bar() -> 1.
