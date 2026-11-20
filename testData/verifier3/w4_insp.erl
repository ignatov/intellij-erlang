-module(w4_insp).
-export([q/1, dup/0, ghost/0, s/0]).

-define(GETTER(N), N() -> N).
-define(UNUSED_IN_BODY(X), begin Unused = 1, X end).
-define(BIND(V, E), V = E).

q(A) -> ?UNUSED_IN_BODY(A).
?GETTER(dup).
?GETTER(dup).
s() -> ?BIND(NeverUsed, 1), ok.
