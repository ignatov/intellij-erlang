-module(v7_multi_a).
-export([shared/0, plain/0]).
-define(DEF(N), N() -> ok).

?DEF(shared).

plain() -> ok.
