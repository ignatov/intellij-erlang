%% Заголовок рядом с файлом: -include("near.hrl") без пути.
%% Плагин и erlc ищут его сначала в папке этого файла.
%%
%% Что попробовать:
%% 1. Поставь курсор на `near.hrl` в -include и нажми Cmd+B.
%% 2. Поставь курсор на `NEAR_PAIR` и нажми Cmd+B.
%% 3. Поставь курсор на `NEAR` в заголовке near.hrl и нажми Alt+F7.
-module(m_near).

-export([pair/1]).

-include("near.hrl").

pair(X) ->
  {?NEAR, ?NEAR_PAIR(X)}.
