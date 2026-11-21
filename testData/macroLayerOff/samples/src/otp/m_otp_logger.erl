%% Заголовок из OTP: kernel/include/logger.hrl.
%% LOG_INFO и LOG_ERROR есть в трёх арностях. Их тела используют
%% DO_LOG и LOCATION, а LOCATION использует ?FUNCTION_NAME.
%%
%% Что попробовать:
%% 1. Поставь курсор на `LOG_INFO` в `start/0` и нажми Cmd+B. Должно
%%    открыться определение с одним аргументом.
%% 2. То же для `LOG_INFO` с двумя аргументами в `stop/1`.
%% 3. Поставь курсор на `LOG_ERROR` и нажми Alt+F7.
%% 4. Набери `?LOG_` в `start/0` и нажми Ctrl+Space.
-module(m_otp_logger).

-export([start/0, stop/1, fail/1]).

-include_lib("kernel/include/logger.hrl").

start() ->
  ?LOG_INFO("started"),
  ok.

stop(Reason) ->
  ?LOG_INFO("stopped: ~p", [Reason]),
  ?LOG_NOTICE(#{event => stop, reason => Reason}),
  ok.

fail(Error) ->
  ?LOG_ERROR("failed: ~p", [Error], #{domain => [m_otp_logger]}),
  ?LOG(warning, "level as an argument"),
  error.
