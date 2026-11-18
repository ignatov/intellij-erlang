-module(macroClauses).
-export([f/1]).

-define(CLAUSES, a -> 1; b -> 2).

f(X) -> case X of ?CLAUSES<error descr="'#', '(', '*', '+', '++', '-', '--', '->', '.', '/', '/=', ':', <, <expression>, '=/=', '=:=', '=<', '==', '>', '>=', '?', and, band, bor, bsl, bsr, bxor, div, or, rem, when, xor or '{' expected, got 'end'"> </error>end.
