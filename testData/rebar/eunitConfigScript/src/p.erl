-module(p).
-export([v/0]).
-ifdef(FROM_SCRIPT).
v() -> script.
-else.
v() -> plain.
-endif.
