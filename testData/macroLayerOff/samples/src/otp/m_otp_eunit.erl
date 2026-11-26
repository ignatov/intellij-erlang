%% Заголовок из OTP: eunit/include/eunit.hrl.
%% Он включает assert.hrl и делает свои макросы: _test, _assertEqual,
%% debugFmt. Функции *_test и *_test_ eunit экспортирует сам.
%%
%% Что попробовать:
%% 1. Поставь курсор на `_test` в `gen_test_/0` и нажми Cmd+B.
%% 2. Поставь курсор на `assertEqual` и нажми Cmd+B. Он пришёл из
%%    assert.hrl через eunit.hrl.
%% 3. Поставь курсор на `debugFmt` и нажми Cmd+B.
%% 4. Проверь, что `simple_test/0` не помечена как неиспользуемая.
-module(m_otp_eunit).

-include_lib("eunit/include/eunit.hrl").

double(X) ->
  2 * X.

simple_test() ->
  ?debugFmt("double(2) = ~p", [double(2)]),
  ?assertEqual(4, double(2)).

gen_test_() ->
  [?_test(?assert(double(1) =:= 2)),
   ?_assertEqual(6, double(3)),
   ?_assertError(badarith, double(a))].
