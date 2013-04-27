lll(N, Aaaa, OOO, Tuple, List, Init2) ->
  <caret>erlang:error(not_implemented).

foo() ->
  lll(1, aaaa, OOO, {}, [], init2(111)).

init2(N) -> N.