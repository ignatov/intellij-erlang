-export([f/2, g/2, h/2, k/2, m/2]).

f(L, B) -> <<X:(ceil(L))>> = B, X.
g(L, B) -> <<X:(erlang:floor(L))>> = B, X.
h(L, B) -> <<X:(<error descr="Illegal pattern">max(L, 1)</error>)>> = B, X.
k(L, B) -> <<X:(erlang:<error descr="Illegal pattern">min(L, 8)</error>)>> = B, X.
m(L, M) -> #{<error descr="Illegal pattern">max(L, 1)</error> := V} = M, V.
