foo(A, B) ->
  AB = A + B,
  X = foo(AB),
  Y = bar(AB + 1),
  {X, Y}.
