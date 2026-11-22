%% Второй файл, который включает include/shared.hrl. Он использует
%% те же макросы, что m_shared_a.erl. Тут удобно проверять поиск и
%% переименование между файлами.
%%
%% Что попробовать:
%% 1. Поставь курсор на `MAX` в `pick/2` и нажми Shift+F6. Имя должно
%%    поменяться в заголовке и в этом файле.
%% 2. Поставь курсор на `RETRIES` и нажми Alt+F7.
%% 3. Набери `#request.` в `retries/1` и нажми Ctrl+Space.
-module(m_shared_b).

-export([pick/2, retries/1, app/0]).

-include("../../include/shared.hrl").

?NAME_FUN.

pick(A, B) ->
  ?MAX(A, B).

retries(R) ->
  R#request.retries + ?RETRIES.

app() ->
  {app_name(), ?WRAP(?TIMEOUT)}.
