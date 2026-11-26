f() ->
  g(<caret>ok),
  try g(1) of
    ok -> 1
  catch
    throw:ok -> 2
  end.
