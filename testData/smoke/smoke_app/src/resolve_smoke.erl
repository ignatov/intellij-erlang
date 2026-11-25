-module(resolve_smoke).
-behaviour(gen_server).

%% Correct code that resolves into Erlang/OTP and into this project: remote calls, a behaviour, -include_lib with its
%% record and macro, -include, local records and macros, and remote types. The release smoke expects no problem here,
%% so a resolve regression shows up as an extra problem (scripts/smoke/expected-smoke_app.txt).
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").
-include("defs_smoke.hrl").

-export([start_link/0, file_size/1, summary/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {calls = 0 :: non_neg_integer()}).
-define(TIMEOUT, 5000).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec file_size(file:filename()) -> {ok, non_neg_integer()} | {error, file:posix() | badarg}.
file_size(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} -> {ok, Size};
        {error, Reason} -> {error, Reason}
    end.

-spec summary([integer()]) -> #{count := non_neg_integer(), total := integer(), text := string()}.
summary(Numbers) ->
    Total = lists:sum(Numbers),
    Text = lists:flatten(io_lib:format("~p numbers, limit ~p", [length(Numbers), ?SMOKE_LIMIT])),
    ?LOG_INFO("summary ~s", [Text]),
    #{count => length(Numbers), total => Total, text => string:trim(Text)}.

init([]) ->
    {ok, #state{}}.

handle_call(calls, _From, #state{calls = Calls} = State) ->
    {reply, Calls, State#state{calls = Calls + 1}, ?TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, maps:get(missing, #{}, undefined), State}.

handle_cast(_Message, State) ->
    {noreply, State}.
