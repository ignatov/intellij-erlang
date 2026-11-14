foo() ->
  case baz(<caret>X = bar()) of
    _ -> X
  end.

bar() -> 1.

baz(X) -> X.
