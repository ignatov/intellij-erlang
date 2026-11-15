-export([run/0]).

-ifdef(PROFILE).
profile(F) -> {profiled, F()}.
-else.
profile(F) -> F().
-endif.

run() -> profile(fun() -> ok end).
