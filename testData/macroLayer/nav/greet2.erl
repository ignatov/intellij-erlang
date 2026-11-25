-module(greet2).
-export([start/1, setup/0, hello/0]).

-define(PAR, ()).
-define(DEF(Name), Name() -> ok).

start(1) ->
  setup?PAR,
  ok;
start(_) ->
  hello().

setup() -> ok.

?DEF(hello).
