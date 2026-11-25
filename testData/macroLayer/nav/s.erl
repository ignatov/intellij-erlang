-module(s).
-export([a/1, b/0, c/1, d/0, e/1, f/1, g/0, made/0, fixed/0, n/0, t/0]).
-record(r, {rf}).
-define(CONST, 1).
-define(CALL(X), target(X)).
-define(BIND(V), V = 1).
-define(X, Xs = 1).
-define(MK(N), N() -> ok).
-define(FIX, fixed() -> ok).
-define(REC(V), #r{rf = V}).
-define(MKREC, -record(mr, {mf})).
-define(MKTYPE, -type mt() :: integer()).
-define(FRAG, ok; f(_) -> frag_target()).
-define(NEST, ?CONST + 1).
?MKREC.
?MKTYPE.
?MK(made).
?FIX.
target(X) -> X.
frag_target() -> ok.
a(Y) -> ?CALL(Y).
b() -> ?BIND(Z), Z + ?CONST.
c(R) -> {?REC(R), R#r.rf, #mr{mf = 1}}.
d() -> {made(), fixed()}.
e(Q) -> ?CALL(target(Q)).
f(0) -> ?FRAG.
g() -> ?X, Xs.
n() -> ?NEST.
-spec t() -> mt().
t() -> 1.
