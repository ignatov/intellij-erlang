test() ->
  X = maybe
    fail()
  else
    ex -> ok;
    ex -> ok
  <caret>