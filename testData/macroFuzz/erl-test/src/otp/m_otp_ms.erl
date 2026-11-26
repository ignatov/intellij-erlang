%% Заголовок из OTP: stdlib/include/ms_transform.hrl.
%% В нём нет макросов: он включает parse transform ms_transform.
%% Здесь макросы свои, и они стоят внутри fun, которую переписывает
%% ets:fun2ms/1.
%%
%% Что попробовать:
%% 1. Поставь курсор на `MIN_AGE` внутри fun и нажми Cmd+B.
%% 2. Поставь курсор на `ROW` в образце fun и нажми Cmd+B.
%% 3. Проверь, что в fun нет красного: erlc принимает файл.
-module(m_otp_ms).

-export([adults/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(MIN_AGE, 18).
-define(ROW(Name, Age), {person, Name, Age}).

adults() ->
  ets:fun2ms(fun(?ROW(Name, Age)) when Age >= ?MIN_AGE -> Name end).
