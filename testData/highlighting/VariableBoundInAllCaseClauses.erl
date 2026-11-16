-module('VariableBoundInAllCaseClauses').
-export([pattern/1, body/1]).

pattern(T) ->
  case T of
    {a, X} -> ok;
    {b, X} -> ok
  end,
  X.

body(T) ->
  case T of
    a -> R = 1;
    _ -> R = 2
  end,
  R.
