f(A, B, C) ->
  AB = A + B,
  X = foo(AB),
  Y = AB + foo(AB),
  {X, Y}.
