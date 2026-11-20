-module(w5_ok).
-export([p1/0, k/0]).

-define(BIND(V, E), V = E).

p1() -> ?BIND(Xa, 1), Xa + 1.
k() -> ok.
