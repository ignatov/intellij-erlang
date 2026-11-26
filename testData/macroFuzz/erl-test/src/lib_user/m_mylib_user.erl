%% Заголовок из библиотеки внутри проекта:
%% -include_lib("mylib/include/mylib.hrl").
%% Библиотека лежит в libs/mylib. Плагин находит её по файлу
%% libs/mylib/src/mylib.app.src. erlc находит её с флагом -I libs.
%%
%% Что попробовать:
%% 1. Поставь курсор на строку в -include_lib и нажми Cmd+B. Должен
%%    открыться libs/mylib/include/mylib.hrl.
%% 2. Поставь курсор на `MYLIB_VERSION` и нажми Cmd+B.
%% 3. Поставь курсор на `depth` в `?MYLIB_CALL(depth)` и нажми Cmd+B.
%%    Должна открыться функция mylib:depth/1.
%% 4. Набери `#mylib_opts{` и нажми Ctrl+Space.
-module(m_mylib_user).

-export([check/0]).

-include_lib("mylib/include/mylib.hrl").

check() ->
  Opts = #mylib_opts{mode = safe},
  {?MYLIB_VERSION, ?MYLIB_CALL(depth)(Opts), Opts#mylib_opts.mode}.
