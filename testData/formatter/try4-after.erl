test() ->
  try
    fail()
  catch
    module:ex -> ok;
    <caret>