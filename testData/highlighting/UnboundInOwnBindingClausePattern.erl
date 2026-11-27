-export([a/1, c/1, d/0, e/0, h/0]).

a(Parent) ->
  {Pid, Ref} = spawn_monitor(fun() -> receive Ref -> Ref end, Parent ! Ref end),
  {Pid, Ref}.

c(X) -> R = fun() -> case X of R -> ok end, R end, R.

d() -> {ok, T} = begin T = 1, {ok, T} end, T.

e() -> receive Ref -> ok end, Ref.

h() -> case <error descr="Variable 'Y' is unbound">Y</error> of Y -> Y end.
