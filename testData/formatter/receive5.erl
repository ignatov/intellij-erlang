test() ->
  receive
    X ->
      ok;
    X2 ->
      ok
  after<caret>