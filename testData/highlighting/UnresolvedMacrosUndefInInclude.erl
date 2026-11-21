-module('UnresolvedMacrosUndefInInclude').
-export([f/0, g/0]).

-ifdef(M).
-include("undef_m.hrl").
f() -> <error descr="Unresolved macros '?M'">?M</error>.
-endif.

-ifdef(N).
-include("undef_m.hrl").
g() -> ?N.
-endif.
