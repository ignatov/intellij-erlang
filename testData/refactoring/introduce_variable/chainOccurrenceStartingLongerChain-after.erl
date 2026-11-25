foo(A, B) ->
  ABN = A + B + 1,
  X = foo(ABN),
  Y = bar(ABN + 2),
  {X, Y}.
