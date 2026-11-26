-export([main/0, main2/0, main3/0, main4/0]).
-include("unused_out_of_sight.hrl").
-define(APPLY(F), F()).
-define(FN, k).
-define(ARGS, 1, 2).
main() -> ?APPLY(g).
main2() -> ?FN().
main3() -> ?CALL.
main4() -> g2(?ARGS).
<warning descr="Unused function 'g/0'">g</warning>() -> h().
h() -> ok.
<warning descr="Unused function 'k/0'">k</warning>() -> m().
m() -> ok.
<warning descr="Unused function 'g3/0'">g3</warning>() -> h3().
h3() -> ok.
<warning descr="Unused function 'start/0'">start</warning>() -> helper().
helper() -> ok.
<warning descr="Unused function 'local/0'">local</warning>() -> helper2().
helper2() -> ok.
g2(A, B) -> h2(A, B).
h2(A, _) -> A.
