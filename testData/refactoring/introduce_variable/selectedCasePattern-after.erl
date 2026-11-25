f(X) ->
  g(ok),
  case X of
    <caret>Ok = ok -> 1;
    _ -> 2
  end.
