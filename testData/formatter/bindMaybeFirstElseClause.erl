test() ->
  X = maybe
    fail()
  else
    ex -> ok;<caret>