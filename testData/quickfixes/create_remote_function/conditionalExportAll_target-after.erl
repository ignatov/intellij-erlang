-module(conditionalExportAll_target).
-ifdef(TEST).
-compile(export_all).
-endif.
-export([bar/0, f/1]).

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
