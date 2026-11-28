-export([f/2, g/2, h/2, k/2]).

f(L, B) -> <<X:(<error descr="Illegal pattern">ceil(L)</error>)>> = B, X.
g(L, B) -> <<X:(erlang:<error descr="Illegal pattern">floor(L)</error>)>> = B, X.
h(L, B) -> <<X:(<error descr="Illegal pattern">max(L, 1)</error>)>> = B, X.
k(L, M) -> #{<error descr="Illegal pattern">min(L, 1)</error> := V} = M, V.
