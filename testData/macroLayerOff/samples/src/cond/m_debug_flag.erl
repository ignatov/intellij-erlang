%% Макросы с командной строки: -DDEBUG и -DLEVEL=3.
%% В файле нет -define(DEBUG).
%% erlc без флагов: DEBUG не определён, TRACE(X) раскрывается в ok,
%% LEVEL берётся из -define ниже и равен 1.
%% erlc -DDEBUG -DLEVEL=3: TRACE(X) печатает X, LEVEL равен 3.
%% check.sh компилирует файл обоими способами.
%%
%% Что попробовать:
%% 1. Поставь курсор на `TRACE` в `run/1` и нажми Cmd+B. Определений два.
%% 2. Поставь курсор на `LEVEL` и нажми Cmd+B. В файле одно определение,
%%    в ветке -ifndef(LEVEL).
%% 3. Посмотри, как плагин показывает ветку -ifdef(DEBUG).
-module(m_debug_flag).

-export([run/1, level/0]).

-ifdef(DEBUG).
-define(TRACE(X), io:format("~p: ~p~n", [?FUNCTION_NAME, X])).
-else.
-define(TRACE(X), ok).
-endif.

-ifndef(LEVEL).
-define(LEVEL, 1).
-endif.

run(X) ->
  ?TRACE(X),
  X.

level() ->
  ?LEVEL.
