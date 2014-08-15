f() ->
  case dothing() of
    V1 ->
      V2 = 1;
    _ ->
      V2 = 2
  end,
  V<caret>2.
