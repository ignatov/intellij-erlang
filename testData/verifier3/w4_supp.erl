-module(w4_supp).
-export([t1/1, t2/1, t3/0, t4/1]).

-define(CL, a -> 1; b -> 2).

t1(X) -> case X of ?CL end, foo bar.
t2(X) -> case X of ?CL end.
t3() -> foo bar.
t4(X) -> Y = case X of ?CL end Z = 2, {Y, Z}.
