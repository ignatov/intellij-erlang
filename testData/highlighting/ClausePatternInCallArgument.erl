-module('ClausePatternInCallArgument').
-export([f/1, g/1, h/1, k/1, id/1]).
f(A) ->
    id(case A of <warning descr="Unused variable 'Y'">Y</warning> -> [] end).
g(A) ->
    id(case A of {Y} -> Y; A -> ok end).
h(A) ->
    id(receive <warning descr="Unused variable 'Y'">Y</warning> -> A end).
k(A) ->
    id(try A catch _:<warning descr="Unused variable 'Y'">Y</warning> -> ok end).
id(A) -> A.
