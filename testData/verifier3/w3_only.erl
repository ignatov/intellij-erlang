-module(w3_only).
-export([o1/0]).

-ifdef(NEVER_DEFINED_W3).
-define(ONLYIF(V), V = 1).
-endif.
o1() -> ?ONLYIF(Ov), Ov.
