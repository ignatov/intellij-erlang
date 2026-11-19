-export([g/0]).

-ifdef(X).
-export([h/0]).
f() -> 1.
g() -> f().
h() -> 1.
-else.
<warning>f</warning>() -> 2.
g() -> 3.
<warning>h</warning>() -> 2.
-endif.
