-export([f/2, g/2]).

f(X, Y) when ceil(X) > 1, floor(Y) < 5, <error descr="Illegal guard expression">max(X, Y)</error> > 0, <error descr="Illegal guard expression">min(X, Y)</error> < 9 -> ok.

g(X, Y) when erlang:ceil(X) > 1, erlang:floor(Y) < 5, erlang:<error descr="Illegal guard expression">max(X, Y)</error> > 0, erlang:<error descr="Illegal guard expression">min(X, Y)</error> < 9 -> ok.
