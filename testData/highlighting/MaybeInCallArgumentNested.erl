-module('MaybeInCallArgumentNested').
-export([f/1, g/1, id/1]).
f(A) ->
    id(try maybe <<X:8>> ?= A, X end after ok end).
g(A) ->
    id(maybe {X, <warning descr="Unused variable 'Y'">Y</warning>} ?= A, id(X) end).
id(A) -> A.
