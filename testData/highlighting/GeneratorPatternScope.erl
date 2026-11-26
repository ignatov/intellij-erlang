-module('GeneratorPatternScope').
-export([g/1, shadow/1, map/0, bin/0, later/1, earlier/1, width/2, key/2, nested/1]).
g(L) ->
    [L || L <- L].
shadow(<warning descr="Unused variable 'L'">L</warning>) ->
    [L || L <- [1]].
map() ->
    [ok || <warning descr="Unused variable 'K'">K</warning> := _ <- <error descr="Variable 'K' is unbound">K</error>].
bin() ->
    [ok || <<<warning descr="Unused variable 'K'">K</warning>>> <= <error descr="Variable 'K' is unbound">K</error>].
later(L) ->
    [ok || X <- <error descr="Variable 'Y' is unbound">Y</error>, <warning descr="Unused variable 'Y'">Y</warning> <- L, X > 0].
earlier(L) ->
    [Y || X <- L, Y <- X].
width(S, B) ->
    [X || <<X:S>> <= B].
key(K, L) ->
    [V || #{K := V} <- L].
nested(L) ->
    [ok || <warning descr="Unused variable 'K'">K</warning> <- [K || K <- L]].
