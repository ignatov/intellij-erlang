-export([g/1_0, k/1, <warning descr="Function 'k/1' has been already exported.">k/0_1</warning>, h/0]).
-compile({no_auto_import, [size/0_1]}).

g(_, _, _, _, _, _, _, _, _, _) -> ok.
k(X) -> X.
size(X) -> X.
h() -> {fun g/1_0, fun ?MODULE:k/0_1, size(1)}.
