f() ->
  case dothing() of
    V1 ->
      Bar = 1;
    _ ->
      Bar = 2
  end,
  B<caret>ar.
