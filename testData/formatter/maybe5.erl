test() ->
  maybe
    fail()
  else
    ex -> ok;
    ex -> ok<caret>