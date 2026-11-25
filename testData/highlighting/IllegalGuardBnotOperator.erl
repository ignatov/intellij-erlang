-module('IllegalGuardBnotOperator').
-export([f/1, g/1]).

f(X) when erlang:'bnot'(X) =:= -1 -> ok.
g(X) when erlang:<error descr="Illegal guard expression">'bnot'(X, X)</error> =:= -1 -> ok.
