-module('830FunArgument').

foo() ->
  lists:foldl(fun(_FirstParameter, _SecondParameter) -> ok end, [], []).
