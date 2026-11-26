%% Ошибка: макрос определён второй раз без -undef.
%% С -undef между ними ошибки нет (см. local/m_redefine.erl).
%% erlc отвергает файл:
%% erlc: err_redefine.erl:11:9: redefining macro 'SIZE'
%% Здесь должно быть красное на втором `-define(SIZE, 2).`
-module(err_redefine).

-compile([export_all, nowarn_export_all]).

-define(SIZE, 1).
-define(SIZE, 2).

f() ->
  ?SIZE.
