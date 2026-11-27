-export([start/0, stop/0, setup/0]).

-define(PAR, ()).
-define(DEF(N), N() -> ok).

start() -> setup?PAR, hello().

stop() -> setup?PAR, hello().

setup() -> ok.

?DEF(hello).
