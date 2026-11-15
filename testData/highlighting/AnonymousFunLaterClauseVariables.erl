-export([a/0, b/0, c/1]).

a() ->
  fun (0) -> ok;
      (1) -> X = 1, X
  end.

b() ->
  fun (0) -> ok;
      (1) -> {A, B} = {1, 2}, A + B;
      (2) -> Y = 2, case Y of 2 -> Y end
  end.

c(L) ->
  lists:map(fun ({a, V}) -> V; ({b, V}) -> W = V + 1, W end, L).
