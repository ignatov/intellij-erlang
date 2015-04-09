test() ->
  X = receive
    _ -> ok
    after
    1 -> timeout
  <caret>