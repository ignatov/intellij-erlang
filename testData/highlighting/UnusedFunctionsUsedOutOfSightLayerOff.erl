-export([main/0]).
-define(ARGS, 1, 2).
main() -> <warning descr="Unresolved function g/1">g</warning>(?ARGS).
<warning descr="Unused function 'g/2'">g</warning>(A, B) -> h(A, B).
h(A, _) -> A.
