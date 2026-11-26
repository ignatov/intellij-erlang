-module(smoke_app_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    smoke_app_sup:start_link().

stop(_State) ->
    ok.
