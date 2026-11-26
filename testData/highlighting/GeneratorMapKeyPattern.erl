-module('GeneratorMapKeyPattern').
-export([c46/2, f/2]).
c46(L, <warning descr="Unused variable 'K'">K</warning>) -> [ok || K <- L, K <- K, #{K := <warning descr="Unused variable 'K'">K</warning>} <- K].
f(Ks, M) -> [K || K <- Ks, #{K := K} <- M].
