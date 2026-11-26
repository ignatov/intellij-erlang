f() ->
  <caret>Ok = ok,
  g(Ok),
  receive
    ok -> 1
  end.
