list_generators() ->
  [ A || A <- lists:seq(1, 10) ],
  << <<A:8>> || A <- lists:seq(1, 10) >>,
  #{ A => A || A <- lists:seq(1, 10) }.

binary_generators() ->
  B = <<0:64>>,
  [ A || <<A:8>> <= B ],
  << <<A:16>> || <<A:8>> <= B >>,
  #{ A => A || <<A:8>> <= B }.

map_generators() ->
  Map = #{1 => 1, 2 => 2},
  [ {K, V} || K := V <- Map ],
    << <<K:16, V:16>> || K := V <- Map >>,
    #{ K => V || K := V <- Map }.