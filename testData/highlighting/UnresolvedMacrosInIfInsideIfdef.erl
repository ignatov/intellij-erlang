-module('UnresolvedMacrosInIfInsideIfdef').
-export([f1/0, f2/0, g1/0, g2/0]).

-ifndef(M).
-if(false).
f1() -> ok.
-else.
f2() -> <error descr="Unresolved macros '?M'">?M</error>.
-endif.
-endif.

-ifdef(N).
-if(true).
g1() -> ?N.
-endif.
g2() -> ?N.
-endif.
