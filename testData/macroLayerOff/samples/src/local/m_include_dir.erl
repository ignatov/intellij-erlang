%% Заголовок без пути: -include("shared.hrl"), а сам shared.hrl лежит
%% в include/, не рядом с файлом.
%% erlc находит его с флагом -I include (так делает check.sh).
%% Плагин находит его, только если папка include отмечена как папка
%% заголовков: правый клик на include -> Mark Directory as -> Includes Root.
%% Без этой настройки -include здесь не найдётся, и макросы будут красными.
%%
%% Что попробовать:
%% 1. Открой файл без настройки: -include и `RETRIES` должны быть красными.
%% 2. Отметь папку include, как сказано выше. Красное должно уйти.
%% 3. Поставь курсор на `RETRIES` и нажми Cmd+B.
-module(m_include_dir).

-export([retries/0]).

-include("shared.hrl").

retries() ->
  ?RETRIES.
