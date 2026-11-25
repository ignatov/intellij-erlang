f(A, B, C) ->
  AB = A + B,
  X = foo(AB),
  Y = AB + C,
  {X, Y}.
