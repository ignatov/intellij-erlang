-module(def).
-export([f/0, hello/0, world/0, m/1]).
-define(DEF(N), N() -> ok).
-define(WHOLE, world() -> ok).
-define(M(X), m(X) -> X).
?DEF(hello).
?WHOLE.
?M(1);
?M(2).
f() -> hello(), world(), m(1).
