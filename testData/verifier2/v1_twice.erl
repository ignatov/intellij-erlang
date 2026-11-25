-module(v1_twice).
-export([f/1, g/1, h/1]).
-define(TWICE(X), X, X).
-define(CL(N, V), N(V) -> V).

f(A) ->
    Bee = A + 1,
    [?TWICE(Bee)].

?CL(g, 1);
?CL(g, 2).

?CL(h, Var).
