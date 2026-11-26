-export([start/0, setup/0, hello/0]).

-define(PAR, ()).

start() -> setup?PAR, hello().

setup() -> ok.

hello() -> ok.
