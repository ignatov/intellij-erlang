foo() ->
  case NewVar = bar() of
    a -> NewVar;
    _ when NewVar =:= b -> fun_c(Socket = NewVar), Socket
  end,
  NewVar.
