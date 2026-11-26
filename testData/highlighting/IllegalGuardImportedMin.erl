-compile({no_auto_import, [min/2]}).
-import(lists, [min/2]).
-export([f/2]).

f(X, Y) when <error descr="Call to local/imported function min/2 is illegal in guard">min(X, Y)</error> > 0 -> ok.
