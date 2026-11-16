-module('VariableBoundInCaseHead').
-export([head/0]).

head() ->
  case A = 1 of
    1 -> A;
    _ -> stuff
  end.
