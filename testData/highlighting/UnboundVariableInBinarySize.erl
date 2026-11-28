-module('UnboundVariableInBinarySize').
-export([k/2]).

k(Len, <<D:<error descr="Variable 'Len' is unbound">Len</error>/binary>>) -> D.
