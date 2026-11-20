-module(w3_bad).
-export([bad/0]).

-define(BAD(V), V = = 1).
bad() -> ?BAD(Bx), Bx.
