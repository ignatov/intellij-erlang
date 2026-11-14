foo() ->
  case begin bar() end of
    _ -> ok
  end.

bar() -> 1.
