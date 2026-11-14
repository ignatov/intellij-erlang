foo(L) ->
  case [begin {X, X} end || X <- L] of
    R -> R
  end.
