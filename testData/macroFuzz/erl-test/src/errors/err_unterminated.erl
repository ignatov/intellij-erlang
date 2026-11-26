%% Ошибка: список аргументов макроса не закрыт: нет `)`.
%% erlc отвергает файл:
%% erlc: err_unterminated.erl:12:4: badly formed argument for macro 'ID'
%% Здесь должно быть красное на `?ID(1.` в функции `f/0`.
-module(err_unterminated).

-compile([export_all, nowarn_export_all]).

-define(ID(X), X).

f() ->
  ?ID(1.

g() ->
  ok.
