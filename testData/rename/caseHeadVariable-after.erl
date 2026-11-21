f() ->
  case Renamed = fun_b() of
    a -> a;
    _ -> Renamed
  end.
