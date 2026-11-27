-export([f/2, k/2]).

f(A, X) -> case X of {A, A} -> ok; _ -> error end.

k(App, L) -> case L of [{App, V}, {App, W}] -> {V, W}; _ -> none end.
