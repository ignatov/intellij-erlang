%% Заголовок библиотеки mylib. Его включают libs/mylib/src/mylib.erl
%% (относительным путём) и src/lib_user/m_mylib_user.erl (через
%% -include_lib).
%%
%% Что попробовать:
%% 1. Поставь курсор на `MYLIB_VERSION` и нажми Alt+F7. Должны найтись
%%    вхождения в обоих файлах.
%% 2. Поставь курсор на `mylib_opts` и нажми Shift+F6.
-ifndef(MYLIB_HRL).
-define(MYLIB_HRL, true).

-define(MYLIB_VERSION, "0.1.0").
-define(MYLIB_CALL(F), mylib:F).
-define(MYLIB_DEFAULT(Key), proplists:get_value(Key, ?MYLIB_DEFAULTS)).
-define(MYLIB_DEFAULTS, [{depth, 3}, {mode, fast}]).

-record(mylib_opts, {depth = 3 :: pos_integer(), mode = fast :: fast | safe}).

-endif.
