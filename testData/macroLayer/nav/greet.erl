-module(greet).
-export([start/0, setup/0, hello/0, bye/0]).

-define(PAR, ()).
-define(DEF(Name), Name() -> ok).
-define(LOG(Msg), io:format("~s~n", [Msg])).

%% Called by the supervisor.
start() ->
  setup?PAR,
  hello(),
  ?LOG("greet started").

setup() ->
  io:format("ready~n").

%% Two handlers made by one macro.
?DEF(hello).
?DEF(bye).
