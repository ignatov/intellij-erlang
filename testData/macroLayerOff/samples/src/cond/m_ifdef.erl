%% Условная компиляция: -ifdef(TEST), -else, -endif.
%% Макрос LIMIT определён по-разному в каждой ветке этого файла,
%% макрос MODE в каждой ветке заголовка cond_flags.hrl.
%% erlc без флагов берёт ветку -else: LIMIT = 100, MODE = prod.
%% erlc с -DTEST берёт первую ветку: LIMIT = 1, MODE = test, и есть
%% функция only_in_test/0. check.sh компилирует файл обоими способами.
%%
%% Что попробовать:
%% 1. Поставь курсор на `LIMIT` в `limit/0` и нажми Cmd+B. Посмотри,
%%    какие определения предложит плагин.
%% 2. Поставь курсор на `MODE` и нажми Cmd+B.
%% 3. Поставь курсор на `LIMIT` в ветке -else и нажми Alt+F7.
-module(m_ifdef).

-export([limit/0, mode/0]).

-include("../../include/cond_flags.hrl").
%% Второй раз: защита -ifndef в заголовке не даёт определить MODE дважды.
-include("../../include/cond_flags.hrl").

-ifdef(TEST).
-define(LIMIT, 1).
-export([only_in_test/0]).
only_in_test() -> ?LIMIT.
-else.
-define(LIMIT, 100).
-endif.

limit() ->
  ?LIMIT.

mode() ->
  ?MODE.
