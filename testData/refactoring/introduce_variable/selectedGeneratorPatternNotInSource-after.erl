f(L) ->
  [X || X <- g(1), {<caret>N = 1} <- g(1), X > N].
