-module('MaybeInCallArgument').
-export([f/0, g/1, id/1]).
f() ->
    id(maybe X ?= ok, _ = X end).
g(X) ->
    id(maybe ok else <warning descr="Unused variable 'Y'">Y</warning> -> X end).
id(A) -> A.
