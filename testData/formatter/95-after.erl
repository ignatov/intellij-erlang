-module(aaa).

-spec(roll() -> dices()).
roll() ->
  [crypto:rand_uniform(1, 7) || _ <- lists:seq(1, 5)].