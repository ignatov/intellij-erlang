-module(cnd).
-export([c/1]).
-ifdef(DEBUG).
-define(SEP, ;).
-define(V, debug).
-else.
-define(SEP, ;).
-define(V, release).
-endif.
c(1) -> ?V ?SEP
c(2) -> b.
