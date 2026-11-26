-module(v4_nonexpr).
-export([f/1, g/1, h/1]).
-define(ARROW(A), f(X) A X).
-define(CLAUSE(C), C;).
-define(GUARDED(G), h(X) when G -> X;).

?ARROW(->).

?CLAUSE(g(1) -> one)
g(_) -> other.

?GUARDED(is_integer(X); is_float(X))
h(_) -> none.
