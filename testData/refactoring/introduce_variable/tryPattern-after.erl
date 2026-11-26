f() ->
  <caret>Ok = ok,
  g(Ok),
  try g(1) of
    ok -> 1
  catch
    throw:ok -> 2
  end.
