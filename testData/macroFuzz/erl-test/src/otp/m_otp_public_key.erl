%% Заголовок из OTP: public_key/include/public_key.hrl.
%% Имена записей и макросов тут в кавычках: 'RSAPublicKey', 'pkcs-1'.
%% Заголовок включает ещё два заголовка из public_key.
%%
%% Что попробовать:
%% 1. Поставь курсор на `'pkcs-1'` и нажми Cmd+B.
%% 2. Поставь курсор на `'RSAPublicKey'` и нажми Cmd+B.
%% 3. Поставь курсор на поле `modulus` и нажми Alt+F7.
-module(m_otp_public_key).

-export([key/2, oid/0]).

-include_lib("public_key/include/public_key.hrl").

key(N, E) ->
  #'RSAPublicKey'{modulus = N, publicExponent = E}.

oid() ->
  ?'pkcs-1'.
