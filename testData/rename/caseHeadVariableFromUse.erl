foo() ->
  case VarB = bar() of
    a -> VarB;
    _ when VarB =:= b -> fun_c(Socket = VarB), Socket
  end,
  Var<caret>B.
