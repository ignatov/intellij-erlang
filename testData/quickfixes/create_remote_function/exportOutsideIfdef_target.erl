-module(exportOutsideIfdef_target).
-ifdef(TEST).
-export([t1/0]).
-endif.
-export([bar/0]).

-ifdef(TEST).
t1() -> ok.
-endif.

bar() -> ok.
