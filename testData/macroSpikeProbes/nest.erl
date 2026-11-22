-module(nest).
-export([n/0, n/1, s/0]).
-define(SEP, ;).
-define(TWO(A, B), A ?SEP B).
-define(CL(N), n(N) -> N).
-define(STR(E), {??E, E}).
-define(INNER, 10).
-define(OUTER(Y), ?INNER + Y, Y).
?TWO(?CL(1), ?CL(2)).
n() -> n(1).
s() -> {?STR(1 + 2), [?OUTER(3)]}.
