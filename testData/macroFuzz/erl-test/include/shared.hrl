%% Общий заголовок. Его включают m_shared_a, m_shared_b, m_redefine
%% и m_include_dir.
%%
%% Что попробовать:
%% 1. Поставь курсор на `RETRIES` и нажми Alt+F7. Должны найтись
%%    вхождения в m_shared_a и в m_shared_b.
%% 2. Поставь курсор на `WRAP` и нажми Shift+F6. Имя должно поменяться
%%    здесь и в обоих файлах.
%% 3. Поставь курсор на поле `user` в записи `request` и нажми Alt+F7.
-ifndef(SHARED_HRL).
-define(SHARED_HRL, true).

%% Константы.
-define(RETRIES, 3).
-define(TIMEOUT, 1000).
-define(APP, shared).

%% Макросы с аргументами.
-define(WRAP(X), {?APP, X}).
-define(MAX(A, B), (case (A) > (B) of true -> (A); false -> (B) end)).

%% Обрывки.
-define(PAR, ()).
-define(WITH_TIMEOUT(X), (X, ?TIMEOUT)).

%% Запись.
-record(request, {user :: binary(), retries = ?RETRIES :: integer()}).

%% Функция из макроса.
-define(NAME_FUN, app_name() -> ?APP).

-endif.
