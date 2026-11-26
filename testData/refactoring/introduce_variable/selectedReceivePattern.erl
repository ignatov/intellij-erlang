f() ->
  receive
    {msg, <caret>ok} -> g(ok)
  end.
