%% Заголовок из OTP: stdlib/include/assert.hrl.
%% Тела assert-макросов зависят от -ifdef(NOASSERT): в заголовке два
%% набора определений.
%%
%% Что попробовать:
%% 1. Поставь курсор на `assertEqual` и нажми Cmd+B. Должно открыться
%%    определение из assert.hrl.
%% 2. Поставь курсор на `assertMatch` и нажми Cmd+B. Определений два:
%%    одно для NOASSERT, другое без него.
%% 3. Набери `?assert` в `check/1` и нажми Ctrl+Space.
-module(m_otp_assert).

-export([check/1]).

-include_lib("stdlib/include/assert.hrl").

check(X) ->
  ?assert(is_integer(X)),
  ?assertEqual(X, X + 0),
  ?assertMatch({ok, _}, lookup(X)),
  ?assertNot(X < 0),
  ?assertError(badarith, X / 0),
  ok.

lookup(X) when X > 0 -> {ok, X};
lookup(_) -> error.
