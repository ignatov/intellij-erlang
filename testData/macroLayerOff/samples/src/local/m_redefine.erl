%% Макрос из заголовка переопределён в файле: сначала -undef,
%% потом новый -define. Выше -undef действует старое определение
%% из shared.hrl, ниже -define действует новое.
%%
%% Что попробовать:
%% 1. Поставь курсор на `TIMEOUT` в `old/0` и нажми Cmd+B. Должен открыться
%%    shared.hrl: там значение 1000.
%% 2. Поставь курсор на `TIMEOUT` в `new/0` и нажми Cmd+B. Должен открыться
%%    `-define(TIMEOUT, 10).` в этом файле.
%% 3. Поставь курсор на `TIMEOUT` в `-define` этого файла и нажми Alt+F7.
%%    Вхождение в `old/0` не должно найтись.
-module(m_redefine).

-export([old/0, new/0]).

-include("../../include/shared.hrl").

old() ->
  ?TIMEOUT.

-undef(TIMEOUT).
-define(TIMEOUT, 10).

new() ->
  ?TIMEOUT.
