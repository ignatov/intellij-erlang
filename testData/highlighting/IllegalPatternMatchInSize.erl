-export([a/3, b/1, c/1]).

a(L, Y, B) -> <<X:(L + (<error>Y = 8</error>))>> = B, {X, Y}.
b(B) -> <<X:(<error>_Y = 8</error>)>> = B, X.
c(<<L:8, X:(L + (<error>_Y = 8</error>))>>) -> X.
