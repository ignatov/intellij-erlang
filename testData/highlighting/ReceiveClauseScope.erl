-export([outer/1, nested/0, own/0, sibling/0]).

outer(X) -> receive {a, Y} -> X + Y; {b, X} -> X after X -> X end.

nested() -> receive {a, P} -> receive {c, Q} -> P + Q after 0 -> P end; {b, Q} -> Q end.

own() -> receive {a, R} -> R; {b, S} -> S end.

sibling() -> receive {a, W} -> W; {b, _} -> <error descr="Variable 'W' is unbound">W</error> end.
