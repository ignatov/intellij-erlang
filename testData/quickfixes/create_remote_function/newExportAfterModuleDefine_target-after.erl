-define(X, 1).
-module(newExportAfterModuleDefine_target).
-export([f/1]).

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
