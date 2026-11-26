-export([main/0]).
-on_load(init/0).

init() -> load().
load() -> ok.
main() -> F = fun helper/1, F(1).
helper(X) -> helper2(X).
helper2(X) -> X.
-spec spec_only() -> ok.
<warning descr="Unused function 'spec_only/0'">spec_only</warning>() -> dead_callee().
<warning descr="Unused function 'dead_callee/0'">dead_callee</warning>() -> spec_only().
<warning descr="Unused function 'self_loop/0'">self_loop</warning>() -> self_loop().
