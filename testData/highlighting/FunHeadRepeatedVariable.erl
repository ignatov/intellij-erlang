-export([f/1, g/0, h/0, shadow/1, nested/0, clauses/0]).

f(L) ->
  lists:filter(fun({K, K}) -> true; (_) -> false end, L).

g() -> fun(A, A) -> ok end.

h() -> fun(V = [V]) -> V end.

shadow(<warning>K</warning>) -> fun({K, K}) -> ok end.

nested() -> fun(<warning>X</warning>) -> fun(X, X) -> ok end end.

clauses() -> fun(Y) -> Y; ({<warning>Z</warning>, Y}) -> Y end.
