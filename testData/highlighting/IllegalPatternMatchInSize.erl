-export([a/3, b/1, c/1, d/2, e/1, f/1]).

a(L, Y, B) -> <<X:(L + (<error>Y = 8</error>))>> = B, {X, Y}.
b(B) -> <<X:(<error>_Y = 8</error>)>> = B, X.
c(<<L:8, X:(L + (<error>_Y = 8</error>))>>) -> X.
d(M, K) -> #{(<error>K = 1</error>) := V} = M, {K, V}.
e(M) -> #{(<error>_K = 1</error>) := V} = M, V.
f({A = 1, <<L:8, X:L>> = B, #{k := V = 2}}) -> {A, X, B, V}.
