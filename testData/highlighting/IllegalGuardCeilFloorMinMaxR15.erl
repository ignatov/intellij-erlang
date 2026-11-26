-export([f/2, g/2]).

f(X, Y) when <error descr="Illegal guard expression">ceil(X)</error> > 1, <error descr="Illegal guard expression">floor(Y)</error> < 5, <error descr="Illegal guard expression">max(X, Y)</error> > 0, <error descr="Illegal guard expression">min(X, Y)</error> < 9 -> ok.

g(X, Y) when erlang:<error descr="Illegal guard expression">ceil(X)</error> > 1, erlang:<error descr="Illegal guard expression">floor(Y)</error> < 5, erlang:<error descr="Illegal guard expression">max(X, Y)</error> > 0, erlang:<error descr="Illegal guard expression">min(X, Y)</error> < 9 -> ok.
