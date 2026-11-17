-module(nestedMacroClauses).
-export([f/1]).

-define(CLAUSES(A), a -> A; b -> 2).

f(X) -> <weak_warning descr="'case' expression has only one clause">case</weak_warning> X of a -> case X of ?CLAUSES(1)<error descr="'#', '(', '*', '+', '++', '-', '--', '->', '/', '/=', <, '=/=', '=:=', '=<', '==', '>', '>=', '?', and, band, bor, bsl, bsr, bxor, div, or, rem, when or xor expected, got 'end'"> </error>end end.
