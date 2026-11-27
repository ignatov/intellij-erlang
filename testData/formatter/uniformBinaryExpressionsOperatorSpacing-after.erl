-export([f/2, g/3]).
f(A, B) -> X = A - B, X.
g(A, B, C) -> A / B + C.
h(P, A, B, L) -> P ! A / B, L ++ [A - B], A - B * A, A * B - A, X = -A / B, fun f/2.
b(X) -> <<X:8/integer-unit:1>>.
