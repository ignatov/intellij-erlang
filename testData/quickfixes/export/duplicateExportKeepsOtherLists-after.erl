-module(m).
-export([
    %% API
    start/0,
    stop/0
]).
-export([init/1]).

start() -> ok.
stop() -> ok.
init(_) -> ok.
