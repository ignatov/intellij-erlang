foo(A) ->
  if
    A -> <caret>X = 1, X;
    true -> X = 2, X
  end.
