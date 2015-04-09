test() ->
  X = receive
    1 -> ok;
    2 -> ok
  <caret>