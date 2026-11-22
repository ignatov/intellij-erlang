%% Ошибка: макрос NOPE нигде не определён.
%% erlc отвергает файл:
%% erlc: err_undefined.erl:10:4: undefined macro 'NOPE'
%% Здесь должно быть красное на `?NOPE` в функции `f/0`.
-module(err_undefined).

-compile([export_all, nowarn_export_all]).

-define(NOPE, macro_body).

f() ->
  ?NOPE.
