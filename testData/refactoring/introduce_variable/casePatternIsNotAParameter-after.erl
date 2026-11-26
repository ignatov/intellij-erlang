f(X) ->
  <caret>Ok = ok,
  g(Ok),
  case X of
    ok -> 1;
    _ -> 2
  end.
