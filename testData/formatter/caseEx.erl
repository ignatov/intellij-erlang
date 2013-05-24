foo(X) -> case X of 
12 ->
 ok;
13 -> 
Y = case X of 
  13 -> 1;
  _ -> 0
  end,
  Y
  end.