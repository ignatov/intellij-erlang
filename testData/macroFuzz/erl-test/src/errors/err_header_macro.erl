%% Ошибка из заголовка: макрос HALF из include/bad_macros.hrl кончается
%% оператором `div` без правой части. Определение erlc принимает,
%% ошибка появляется в месте вхождения.
%% erlc отвергает файл:
%% erlc: err_header_macro.erl:15:11: syntax error before: '.'
%% Здесь должно быть красное на `?HALF(4)` в функции `f/0`. erlc ставит
%% ошибку на точку после него: там кончилось выражение `4 div`.
-module(err_header_macro).

-compile([export_all, nowarn_export_all]).

-include("../../include/bad_macros.hrl").

f() ->
  ?HALF(4).
