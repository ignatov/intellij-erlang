%% Ошибка: аргумент не подходит телу макроса. SUM ждёт два выражения,
%% а второй аргумент `end` не выражение.
%% erlc отвергает файл:
%% erlc: err_bad_argument.erl:13:11: syntax error before: 'end'
%% Здесь должно быть красное на `end` в `?SUM(1, end)`, функция `f/0`.
-module(err_bad_argument).

-compile([export_all, nowarn_export_all]).

-define(SUM(A, B), A + B).

f() ->
  ?SUM(1, end).
