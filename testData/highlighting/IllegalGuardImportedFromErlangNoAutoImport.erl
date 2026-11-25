-compile(no_auto_import).
-import(erlang, [length/1, is_list/1]).
-export([f/1]).

f(X) when is_list(X), length(X) > 0 -> ok.
