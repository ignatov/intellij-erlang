-module(ti_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Server = {ti_server, {ti_server, start_link, []}, permanent, 2000, worker, [ti_server]},
  {ok, {{one_for_one, 0, 1}, [Server]}}.
