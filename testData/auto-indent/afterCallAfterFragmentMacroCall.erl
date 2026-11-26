-module(greet).
-export([start/0, setup/0, hello/0]).

-define(PAR, ()).

start() ->
  setup?PAR,
  hello(),<caret>
  ok.

setup() -> ok.
hello() -> ok.
