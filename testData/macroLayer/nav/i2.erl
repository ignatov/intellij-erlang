-module(i2).
-include("h.hrl").
-export([a/1, b/0, c/1, d/0, e/1, hf/1, g/0, hmade/0, hfixed/0, n/0, t/0]).
?H_MKREC.
?H_MKTYPE.
?H_MK(hmade).
?H_FIX.
h_target(X) -> X.
h_frag_target() -> i2.
a(Y) -> ?H_CALL(Y).
b() -> ?H_BIND(Z), Z + ?H_CONST.
c(R) -> {?H_REC(R), R#hr.hf, #hmr{hmf = 1}}.
d() -> {hmade(), hfixed()}.
e(Q) -> ?H_CALL(h_target(Q)).
hf(0) -> ?H_FRAG.
g() -> ?H_X, Xh.
n() -> ?H_NEST.
-spec t() -> hmt().
t() -> 1.
