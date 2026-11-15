-module(remoteCallNoExport_target).

-export([lll/0]).

bar() -> ok.

lll() ->
  erlang:error(not_implemented).
