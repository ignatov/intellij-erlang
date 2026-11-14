foo() ->
  case receive _ -> bar() end of
    _ -> ok
  end.

bar() -> 1.
