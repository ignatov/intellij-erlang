f() ->
  receive
    {msg, <caret>Ok = ok} -> g(Ok)
  end.
