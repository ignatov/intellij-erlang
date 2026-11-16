-module('VariableUnboundInOwnMatch').
-export([pair/0]).

pair() ->
  {A, B} = {1, <error descr="Variable 'A' is unbound">A</error> + 1},
  B.
