-module(exportOutsideIfdef_target).
-ifdef(TEST).
-export([t1/0]).
-endif.
-export([bar/0, f/1]).

-ifdef(TEST).
t1() -> ok.
-endif.

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
