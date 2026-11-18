foo() ->
  case Var<caret>B = bar() of
    a -> VarB;
    VarB -> ok;
    _ when VarB =:= b -> ok
  end,
  VarB.
