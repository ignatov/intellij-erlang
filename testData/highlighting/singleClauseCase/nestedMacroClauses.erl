-module(nestedMacroClauses).
-export([f/1]).

-define(CLAUSES(A), a -> A; b -> 2).

f(X) -> <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of a -> case X of ?CLAUSES(1) end end.
