-module(v3_nest).
-export([n/1, m/1, t/1, helper/0]).
-define(INNER(X), X + 1).
-define(WRAP(X), {X}).
-define(CL(N, V), N(V) -> V).
-define(TAIL(E), E;).
-define(OUTER(N, B), N(Arg) -> ?INNER(Arg) + B;).

?OUTER(n, 10)
n(_) -> 0.

?CL(m, ?WRAP(Www)).

t(1) -> ?TAIL(?INNER(helper()))
t(_) -> 0.

helper() -> 1.
