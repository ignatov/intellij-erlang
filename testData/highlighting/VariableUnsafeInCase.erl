-module('VariableUnsafeInCase').
-export([some/1]).

some(T) ->
  case T of
    a -> R = 1;
    _ -> ok
  end,
  <error descr="Variable 'R' is unsafe in 'case'">R</error>.
