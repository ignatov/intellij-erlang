%% Заголовок из OTP: stdlib/include/qlc.hrl.
%% Он включает parse transform qlc. Макросы стоят внутри
%% списочного выражения запроса.
%%
%% Что попробовать:
%% 1. Поставь курсор на `LIMIT` внутри запроса и нажми Cmd+B.
%% 2. Поставь курсор на `ITEM` и нажми Alt+F7.
-module(m_otp_qlc).

-export([small/1]).

-include_lib("stdlib/include/qlc.hrl").

-define(LIMIT, 10).
-define(ITEM(K, V), {K, V}).

small(Table) ->
  Q = qlc:q([K || ?ITEM(K, V) <- ets:table(Table), V < ?LIMIT]),
  qlc:e(Q).
