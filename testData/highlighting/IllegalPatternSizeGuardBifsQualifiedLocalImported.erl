-export([f/2, g/2, h/2, k/2, m/2]).
-compile({no_auto_import, [max/2, min/2]}).
-import(foo, [min/2]).

f(L, B) -> <<X:(erlang:ceil(L))>> = B, X.
g(L, B) -> <<X:(erlang:max(L, 1))>> = B, X.
h(L, M) -> #{erlang:min(L, 1) := V} = M, V.
k(L, B) -> <<X:(<error descr="Illegal pattern">max(L, 1)</error>)>> = B, X.
m(L, B) -> <<X:(<error descr="Illegal pattern">min(L, 1)</error>)>> = B, X.

max(A, _) -> A.
