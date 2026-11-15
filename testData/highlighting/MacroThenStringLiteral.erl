-define(M, "m").

-export([module_string/0, user_macro/0, unresolved/0, stringified/0, stringified_after/0]).

module_string() -> ?MODULE_STRING":f" ?M.
user_macro() -> ?M "x".
unresolved() -> <error descr="Unresolved macros '?UNDEFINED'">?UNDEFINED</error> "x".

-define(S(X), ??X " failed").
stringified() -> ?S(a).
-define(L(X), io:format(?MODULE_STRING ": " ??X " = ~p~n", [X])).
-define(T(X), ?M "x" ??X).
stringified_after() -> ?L(1), ?T(a).
