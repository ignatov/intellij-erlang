h(X) ->
  case X of
    ok -> 1;
    _ -> <caret>Ok = ok,
      g(Ok)
  end.
