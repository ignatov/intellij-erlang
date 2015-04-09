test() ->
  X = try
    fail()
  catch
    module:ex -> ok;
    module:ex -> ok
  <caret>