-module(macroClauses).
-export([f/1]).

-define(CLAUSES, a -> 1; b -> 2).

f(X) -> case X of ?CLAUSES end.
