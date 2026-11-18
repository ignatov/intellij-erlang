foo() ->
  case if true -> bar() end of
    _ -> ok
  end.

bar() -> 1.
