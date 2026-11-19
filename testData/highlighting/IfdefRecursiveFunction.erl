-export([run/0]).

-ifdef(X).
<warning>f</warning>(N) -> N.
-else.
<warning>f</warning>(0) -> 0;
f(N) -> f(N - 1).
-endif.

run() -> ok.
