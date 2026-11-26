-module(m).
-export([
    %% API
    start/0,
    % callback
    stop/0
]).
-export([start/0 % again
         ]).
-export([init<caret>/1]).

start() -> ok.
stop() -> ok.
init(_) -> ok.
