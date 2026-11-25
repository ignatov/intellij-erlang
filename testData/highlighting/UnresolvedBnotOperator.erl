-module('UnresolvedBnotOperator').
-export([op/0]).

op() ->
  F = fun erlang:'bnot'/1,
  {F, erlang:'bnot'(1), fun erlang:<warning>'bnot'</warning>/2}.
