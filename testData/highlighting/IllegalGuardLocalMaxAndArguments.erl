-compile({no_auto_import, [max/2]}).
-export([max/2, f/2, g/2]).

max(X, Y) -> {X, Y}.

f(X, Y) when <error descr="Call to local/imported function max/2 is illegal in guard">max(X, Y)</error> > 0 -> ok.

g(X, Y) when min(<error descr="Illegal guard expression">X = 1</error>, Y) < 9, erlang:ceil(<error descr="Illegal guard expression">X ! Y</error>) > 1 -> ok.
