test() ->
  receive
    X ->
      ok;
    X2 ->
      ok
  after
    1000 ->
      ok
  <caret>