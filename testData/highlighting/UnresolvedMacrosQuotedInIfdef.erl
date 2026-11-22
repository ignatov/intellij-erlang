-module('UnresolvedMacrosQuotedInIfdef').
-export([f/0, g/0]).

-ifdef('M').
f() -> ?M.
-endif.

-ifdef(N).
g() -> ?'N'.
-endif.
