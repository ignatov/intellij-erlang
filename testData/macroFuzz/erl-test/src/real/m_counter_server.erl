%% Код как в реальном проекте: gen_server с макросами логирования из
%% kernel/include/logger.hrl и записью из своего заголовка counter.hrl.
%%
%% Что попробовать:
%% 1. Поставь курсор на `counter` в `#counter{}` и нажми Cmd+B: запись из
%%    include/counter.hrl.
%% 2. Поставь курсор на `CALL` в `get/0` и нажми Cmd+B. Потом Alt+F7 на
%%    `CALL` в заголовке: должны найтись вхождения и в этом файле, и в
%%    m_counter_server_tests.erl.
%% 3. Поставь курсор на `LOG_WARNING` в `handle_call/3` и нажми Cmd+B.
%% 4. Поставь курсор на `STATE_VALUE` и нажми Shift+F6.
%% 5. Набери `State#counter.` в `handle_cast/2` и нажми Ctrl+Space.
-module(m_counter_server).

-behaviour(gen_server).

-export([start_link/1, get/0, bump/0, set_step/1, reset/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include("../../include/counter.hrl").

%% Обрывок: начало кортежа ответа gen_server.
-define(REPLY(Value), {reply, Value,).
-define(STATE_VALUE(S), S#counter.value).
-define(IS_SMALL(V), V < ?MAX_VALUE).

%% Клиент.

start_link(Name) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Name, []).

get() ->
  ?CALL(get).

bump() ->
  ?CALL(bump).

set_step(Step) when is_integer(Step), Step > 0 ->
  ?CAST({set_step, Step}).

reset() ->
  ?CAST(reset).

stop() ->
  gen_server:stop(?SERVER).

%% Сервер.

init(Name) ->
  ?LOG_INFO("counter ~p started", [Name]),
  {ok, #counter{name = Name}}.

handle_call(get, _From, State) ->
  ?REPLY(?STATE_VALUE(State)) State};
handle_call(bump, _From, #counter{value = V, step = S} = State)
    when ?IS_SMALL(V + S) ->
  New = V + S,
  ?LOG_DEBUG("bump ~p -> ~p", [V, New]),
  ?REPLY(New) State#counter{value = New,
                             history = [V | State#counter.history]}};
handle_call(bump, _From, State) ->
  ?LOG_WARNING("counter ~p is full", [State#counter.name]),
  ?REPLY({error, full}) State};
handle_call(Other, _From, State) ->
  ?LOG_ERROR(#{unexpected => Other, in => ?FUNCTION_NAME}),
  {reply, {error, unknown}, State}.

handle_cast({set_step, Step}, State) ->
  {noreply, State#counter{step = Step}};
handle_cast(reset, State) ->
  ?LOG_NOTICE("reset at ~p", [?STATE_VALUE(State)]),
  {noreply, State#counter{value = 0, history = []}}.

handle_info(Info, State) ->
  ?LOG_INFO("ignored ~p", [Info]),
  {noreply, State}.

terminate(Reason, #counter{name = Name}) ->
  ?LOG_INFO("counter ~p stopped: ~p", [Name, Reason]),
  ok.
