%%%-------------------------------------------------------------------
%% @doc rebar3app public API
%% @end
%%%-------------------------------------------------------------------

-module(rebar3app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rebar3app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
