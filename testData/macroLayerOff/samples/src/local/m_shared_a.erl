%% Первый из двух файлов, которые включают один заголовок
%% include/shared.hrl. Путь к нему относительный: ../../include/shared.hrl.
%% Второй файл: m_shared_b.erl.
%%
%% Что попробовать:
%% 1. Поставь курсор на `RETRIES` в `start/1` и нажми Cmd+B. Должен
%%    открыться заголовок shared.hrl.
%% 2. Нажми Alt+F7 на `RETRIES`. Должны найтись вхождения и в этом файле,
%%    и в m_shared_b.erl.
%% 3. Поставь курсор на `wait` в `wait?WITH_TIMEOUT(Ref)` и нажми Cmd+B.
%% 4. Поставь курсор на `app_name` в `app_name()` и нажми Cmd+B: функция
%%    сделана макросом `?NAME_FUN` из заголовка.
-module(m_shared_a).

-export([start/1]).

-include("../../include/shared.hrl").

?NAME_FUN.

start(User) ->
  R = #request{user = User},
  Ref = make_ref(),
  {?WRAP(R#request.retries), ?RETRIES, wait?WITH_TIMEOUT(Ref),
   app_name(), ping?PAR}.

wait(Ref, Timeout) ->
  {Ref, Timeout}.

ping() ->
  pong.
