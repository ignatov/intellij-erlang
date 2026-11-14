foo() ->
  case Var<caret>B = bar() of
    a -> VarB;
    _ when VarB =:= b -> fun_c(Socket = VarB), Socket
  end,
  VarB.
