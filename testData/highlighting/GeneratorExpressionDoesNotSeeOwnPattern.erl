-module('GeneratorExpressionDoesNotSeeOwnPattern').
-export([f/0, g/1]).
f() ->
    [ok || <warning descr="Unused variable 'K'">K</warning> <- <error descr="Variable 'K' is unbound">K</error>].
g(S) ->
    [ok || <warning descr="Unused variable 'S'">S</warning> <- (S = [])].
