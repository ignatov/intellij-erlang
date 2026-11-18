foo(L) ->
  case [begin <caret>Y = X, {Y, Y} end || X <- L] of
    R -> R
  end.
