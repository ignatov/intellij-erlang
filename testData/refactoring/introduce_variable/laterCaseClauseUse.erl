h(X) ->
  case X of
    ok -> 1;
    _ -> g(<caret>ok)
  end.
