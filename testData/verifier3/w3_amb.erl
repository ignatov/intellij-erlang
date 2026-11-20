-module(w3_amb).
-export([a1/0, r1/0, r2/0, o1/0]).

-ifdef(TEST).
-define(MK(V), V = test).
-else.
-define(MK(V), V = prod).
-endif.

a1() -> ?MK(Am), Am.

-define(RD(V), V = 1).
r1() -> ?RD(Ra), Ra.
-undef(RD).
-define(RD(V), V = 2).
r2() -> ?RD(Rb), Rb.

-ifdef(NEVER_DEFINED_W3).
-define(ONLYIF(V), V = 1).
-endif.
o1() -> ok.
