foo() ->
  case bar(fun() -> 1 end) of
    F -> F()
  end.

bar(F) -> F.
