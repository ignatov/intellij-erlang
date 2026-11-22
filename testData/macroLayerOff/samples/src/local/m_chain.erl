%% Цепочка из трёх заголовков: этот файл включает chain_top.hrl,
%% тот включает chain_mid.hrl, а тот включает chain_base.hrl.
%% Все три лежат в include/.
%%
%% Что попробовать:
%% 1. Поставь курсор на `BASE` в `base/0` и нажми Cmd+B. Должен открыться
%%    chain_base.hrl, третье звено.
%% 2. Поставь курсор на `MID` и нажми Cmd+B: chain_mid.hrl.
%% 3. Поставь курсор на `base_rec` и нажми Cmd+B: запись из третьего звена.
%% 4. Поставь курсор на `add` в `?MID_CALL(add)` и нажми Cmd+B. Аргументы
%%    вызова приходят обрывком `BASE_ARGS` из третьего звена.
-module(m_chain).

-export([base/0, top/0, call/0]).

-include("../../include/chain_top.hrl").

base() ->
  {?BASE, #base_rec{}}.

top() ->
  {?TOP, ?MID}.

call() ->
  ?MID_CALL(add).

add(A, B) ->
  A + B.
