-module(w1_map).
-export([p1/0, p2/0, p3/0, p4/0, p5/0, p6/0, p7/1]).

-define(BIND(V, E), V = E).
-define(BIND2(V), {V, V} = {1, 1}).
-define(SWAPBIND(A, B), {B, A} = {1, 2}).
-define(STRBIND(V), V = ??V).
-define(INNERB(V), V = 1).
-define(OUTERB(V), ?INNERB(V)).

p1() -> ?BIND(Xa, 1), Xa + 1.
p2() -> ?BIND2(Xb), Xb.
p3() -> ?SWAPBIND(Xc, Xd), {Xc, Xd}.
p4() -> ?STRBIND(Xe), Xe.
p5() -> ?OUTERB(Xf), Xf.
p6() -> ?BIND(Xg, ?BIND(Xh, 2)), Xg + Xh.
p7(Arg) -> ?BIND(Xi, Arg), {Xi, Arg}.
