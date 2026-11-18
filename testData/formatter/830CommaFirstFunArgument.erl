f() ->
  lists:foldl(fun(_FirstParameter, _SecondParameter) -> ok end, [], []).

g() ->
  {ok, fun(_FirstParameter, _SecondParameter) -> ok end}.
