-module(w3_before).
-export([u1/0]).

u1() -> ?LATER(Ub), Ub.
-define(LATER(V), V = 3).
