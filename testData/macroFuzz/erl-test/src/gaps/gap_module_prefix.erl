%% Известный пробел плагина: имя модуля стоит перед макросом, а
%% двоеточие в макросе: `lists?COLON reverse(L)`. erlc принимает файл:
%% это `lists:reverse(L)`.
%% Плагин такой вызов не поддерживает. Сравни с `?LISTS reverse(L)` в
%% one_file/m_fragments.erl, где `lists:` целиком в макросе.
%%
%% Что попробовать:
%% 1. Посмотри, есть ли красное в `rev/1`. Его быть не должно.
%% 2. Поставь курсор на `reverse` и нажми Cmd+B. Правильный ответ:
%%    lists:reverse/1.
-module(gap_module_prefix).

-export([rev/1]).

-define(COLON, :).

rev(L) ->
  lists?COLON reverse(L).
