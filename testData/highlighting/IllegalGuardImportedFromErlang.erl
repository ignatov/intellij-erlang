-compile({no_auto_import, [abs/1]}).
-import(erlang, [abs/1]).
-export([f/1]).

f(X) when abs(X) > 0 -> ok.
