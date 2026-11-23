-export([f/1, g/1, h/2, i/1, j/1]).

f(<<Len:8, Data:(Len*8)/bits>>) -> Data.
g(M) -> K = 1, #{K+1 := V} = M, V.
h(L, B) -> <<X:(length(L))>> = B, X.
i(<<L:8, X:(-L)>>) -> X.
j(<<Len:8, Data:(Len+1)/binary>>) -> Data.
