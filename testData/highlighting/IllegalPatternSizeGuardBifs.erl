-export([f/2, g/2, h/2, k/2, m/2]).

f(L, B) -> <<X:(ceil(L))>> = B, X.
g(L, B) -> <<X:(floor(L))>> = B, X.
h(L, B) -> <<X:(max(L, 1))>> = B, X.
k(L, B) -> <<X:(min(L, 8))>> = B, X.
m(L, M) -> #{max(L, 1) := V} = M, V.
