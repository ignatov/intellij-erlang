foo() ->
  case bar(fun() -> <caret>X = 1, X end) of
    F -> F()
  end.

bar(F) -> F.
