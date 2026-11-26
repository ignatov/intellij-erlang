%% Код как в реальном проекте: модуль тестов eunit для m_counter_server.
%% Включает eunit.hrl из OTP и свой заголовок counter.hrl.
%% Тут есть фикстура setup, генераторы ?_test и ?_assert*, свои
%% макросы поверх eunit.
%%
%% Что попробовать:
%% 1. Поставь курсор на `_assertEqual` и нажми Cmd+B.
%% 2. Поставь курсор на `CALL` в `call_test_/0` и нажми Cmd+B: он из
%%    include/counter.hrl.
%% 3. Поставь курсор на `WITH_SERVER` и нажми Alt+F7.
%% 4. Поставь курсор на `bump` в `m_counter_server:bump()` и нажми Cmd+B.
%% 5. Набери `?_assert` в `counter_test_/0` и нажми Ctrl+Space.
-module(m_counter_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/counter.hrl").

%% Фикстура: запустить сервер, выполнить тесты, остановить сервер.
-define(WITH_SERVER(Tests),
        {setup,
         fun() -> {ok, Pid} = m_counter_server:start_link(test), Pid end,
         fun(_) -> m_counter_server:stop() end,
         Tests}).

%% Ожидаемое значение после N вызовов bump.
-define(AFTER_BUMPS(N), N * ?DEFAULT_STEP).

counter_test_() ->
  ?WITH_SERVER(
    [?_assertEqual(0, m_counter_server:get()),
     ?_assertEqual(?AFTER_BUMPS(1), m_counter_server:bump()),
     ?_assertEqual(?AFTER_BUMPS(2), m_counter_server:bump()),
     ?_test(begin
              m_counter_server:reset(),
              ?assertEqual(0, m_counter_server:get())
            end)]).

step_test_() ->
  ?WITH_SERVER(
    [?_test(begin
              ok = m_counter_server:set_step(5),
              ?assertEqual(5, m_counter_server:bump())
            end),
     ?_assertError(function_clause, m_counter_server:set_step(0))]).

call_test_() ->
  ?WITH_SERVER(
    [?_assertMatch({error, unknown}, ?CALL(something_else)),
     ?_assert(is_integer(?CALL(get)))]).

record_defaults_test() ->
  C = #counter{name = x},
  ?assertEqual(?DEFAULT_STEP, C#counter.step),
  ?assertEqual([], C#counter.history),
  ?assertNot(C#counter.value > ?MAX_VALUE).
