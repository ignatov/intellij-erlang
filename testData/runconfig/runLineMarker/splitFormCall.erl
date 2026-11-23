-module(greet).
-export([start/0, setup/0, hello/0]).

-define(PAR, ()).
-define(DEF(Name), Name() -> ok).

start() ->
  setup?PAR,
  hello().

setup() ->
  io:format("ready~n").

?DEF(hello).
