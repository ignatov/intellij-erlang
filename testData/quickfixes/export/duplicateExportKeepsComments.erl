-module(m).
-export([
    %% API
    start/0,
    init/1, % callback
    stop/0
]).
-export([start/0, % again
         init/1]).
-export([init<caret>/1]).

start() -> ok.
stop() -> ok.
init(_) -> ok.
