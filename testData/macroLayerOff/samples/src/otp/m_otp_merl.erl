%% Заголовок из OTP: syntax_tools/include/merl.hrl.
%% Макрос Q(Text) раскрывается в merl:quote(?LINE, Text): в теле
%% встроенный макрос LINE.
%%
%% Что попробовать:
%% 1. Поставь курсор на `Q` в `tree/0` и нажми Cmd+B.
%% 2. Нажми Cmd+B на `Q` с двумя аргументами в `tree/1`: должно открыться
%%    другое определение.
-module(m_otp_merl).

-export([tree/0, tree/1]).

-include_lib("syntax_tools/include/merl.hrl").

tree() ->
  ?Q("1 + 2").

tree(Arg) ->
  ?Q("foo(_@arg)", [{arg, Arg}]).
