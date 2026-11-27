-module('UnboundVariableInComputedBinarySize').
-export([f/2, h/1]).

f(Len, <<D:(<error descr="Variable 'Len' is unbound">Len</error>*8)/bits>>) -> D.
h(B) -> <<X:(<error descr="Variable 'N' is unbound">N</error>*8)>> = B, X.
