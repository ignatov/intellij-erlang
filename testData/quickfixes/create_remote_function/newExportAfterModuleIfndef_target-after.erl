-ifndef(X).
-define(X, 1).
-endif.
-module(newExportAfterModuleIfndef_target).
-export([f/1]).

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
