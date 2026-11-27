-export([g/0, t/0, p/0, r/0]).

g() -> receive {a, V} -> V; {b, _} when <error descr="Variable 'V' is unbound">V</error> > 1 -> ok end.

t() -> receive {a, V} -> V after <error descr="Variable 'V' is unbound">V</error> -> ok end.

p() -> receive {'EXIT', <warning descr="Unused variable 'Pid'">Pid</warning>, _} -> ok; {'DOWN', _, _, <warning descr="Unused variable 'Pid'">Pid</warning>, _} -> ok end.

r() -> receive {a, V} -> V; {b, V} -> V + 1 end.
