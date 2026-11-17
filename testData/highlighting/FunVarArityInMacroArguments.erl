-export([f/0, g/0]).
-define(IGNORE(What), ok).
-define(STR(What), ??What).

f() -> ?IGNORE(fun F/1).
g() -> ?STR(fun F/1).
