test() ->
  X = receive
    _ -> ok
      after<caret>