-module('UnresolvedMacrosInIfdef').
-export([x/0]).

-ifdef(MACROS).
-define(X, ?MACROS).
-else.
-define(X, undefined).
-endif.

x() -> ?X.
