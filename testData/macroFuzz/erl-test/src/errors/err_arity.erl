%% Ошибка: неверное число аргументов. ID определён только с одним
%% аргументом, а вызван с двумя.
%% erlc отвергает файл:
%% erlc: err_arity.erl:13:4: argument mismatch for macro 'ID'
%% Здесь должно быть красное на `?ID(1, 2)` в функции `f/0`.
-module(err_arity).

-compile([export_all, nowarn_export_all]).

-define(ID(X), X).

f() ->
  ?ID(1, 2).
