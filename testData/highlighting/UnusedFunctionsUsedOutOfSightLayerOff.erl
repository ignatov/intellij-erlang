-export([main/0]).
-define(ARGS, 1, 2).
main() -> <warning descr="Unresolved function g/1">g</warning>(?ARGS).
<warning descr="Unused function 'g/2'">g</warning>(A, B) -> h(A, B).
h(A, _) -> A.
-export([main2/0]).
-define(A, 0).
main2() -> fun g2/<error descr="integer expected, got '?'">?</error>A<error descr="'.' unexpected">.</error>
<warning descr="Unused function 'g2/0'">g2</warning>() -> h2().
h2() -> ok.
