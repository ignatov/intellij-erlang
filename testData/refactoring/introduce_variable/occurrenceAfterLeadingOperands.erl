f(A, B, C) ->
  X = foo(<selection>A + B</selection>),
  Y = A + B + foo(A + B),
  {X, Y}.
