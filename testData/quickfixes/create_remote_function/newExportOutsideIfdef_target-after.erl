-module(newExportOutsideIfdef_target).

-export([f/1]).

-ifdef(TEST).
-export([t1/0]).
t1() -> ok.
-endif.

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
