recovery() ->
  [ || A <- lists:seq(1, 10) ],
  << || A <- lists:seq(1, 10) >>,
  #{ || A <- lists:seq(1, 10) }.