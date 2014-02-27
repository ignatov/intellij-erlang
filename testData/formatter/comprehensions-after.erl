one_liners() ->
  [A || A <- lists:seq(1, 10)],
  <<<<A:8>> || A <- lists:seq(1, 10)>>,
  #{A => A || A <- lists:seq(1, 10)}.

two_liners() ->
  [A ||
    A <- lists:seq(1, 10)],
  <<<<A:8>> ||
    A <- lists:seq(1, 10)>>,
  #{A => A ||
    A <- lists:seq(1, 10)}.

three_liners() ->
  [A
    ||
    A <- lists:seq(1, 10)],
  <<<<A:8>>
    ||
    A <- lists:seq(1, 10)>>,
  #{A => A
    ||
    A <- lists:seq(1, 10)}.

four_liners() ->
  [A
    ||
    A <- lists:seq(1, 10)
  ],
  <<<<A:8>>
    ||
    A <- lists:seq(1, 10)
  >>,
  #{A => A
    ||
    A <- lists:seq(1, 10)
  }.

five_liners() ->
  [
    A
    ||
    A <- lists:seq(1, 10)
  ],
  <<
    <<A:8>>
    ||
    A <- lists:seq(1, 10)
  >>,
  #{
    A => A
    ||
    A <- lists:seq(1, 10)
  }.