test() ->
  #{A => A || A <- [a, b], B <- [c, d]<caret>}.