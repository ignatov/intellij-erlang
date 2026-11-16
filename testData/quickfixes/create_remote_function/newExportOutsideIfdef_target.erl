-module(newExportOutsideIfdef_target).

-ifdef(TEST).
-export([t1/0]).
t1() -> ok.
-endif.

bar() -> ok.
