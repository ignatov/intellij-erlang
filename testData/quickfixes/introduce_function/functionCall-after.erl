foo() ->
  lll(1, aaaa, OOO, {}, [], init2(111)).

lll(N, Aaaa, OOO, Tuple, List, Init2) ->
  <caret>error(not_implemented).

init2(N) -> N.