%% Заголовок с условной компиляцией.
%% -ifndef(COND_FLAGS_HRL) защищает от двойного включения: m_ifdef.erl
%% включает этот заголовок два раза, и ошибки нет.
%% MODE определён по-разному при TEST и без TEST.
%%
%% Что попробовать:
%% 1. Поставь курсор на `MODE` в m_ifdef.erl и нажми Cmd+B. Определений
%%    два: в ветке -ifdef(TEST) и в ветке -else.
-ifndef(COND_FLAGS_HRL).
-define(COND_FLAGS_HRL, true).

-ifdef(TEST).
-define(MODE, test).
-else.
-define(MODE, prod).
-endif.

-endif.
