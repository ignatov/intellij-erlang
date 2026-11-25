f(X) ->
  case X of
    {a, ok} -> <caret>Ok = ok,
      g(Ok);
    {b, ok} -> 2
  end.
