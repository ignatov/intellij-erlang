-export([start/0, stop/0, setup/0]).

-define(PAR, ()).
-define(DEF(N), N() -> ok).

start() -> setup?PAR, <warning descr="Unused function 'hello/1'"><warning descr="Duplicate function 'hello/1'">hello</warning></warning>(1).

stop() -> setup?PAR, <warning descr="Unused function 'hello/1'"><warning descr="Duplicate function 'hello/1'">hello</warning></warning>(1).

setup() -> ok.

?DEF(hello).
