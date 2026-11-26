f(X) ->
  case X of
    {a, ok} -> g(<caret>ok);
    {b, ok} -> 2
  end.
