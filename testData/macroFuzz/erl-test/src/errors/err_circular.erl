%% Ошибка: макросы ссылаются друг на друга по кругу: A на B, B на A.
%% erlc отвергает файл:
%% erlc: err_circular.erl:13:4: circular macro 'A'
%% Здесь должно быть красное на `?A` в функции `f/0`.
-module(err_circular).

-compile([export_all, nowarn_export_all]).

-define(A, ?B + 1).
-define(B, ?A + 1).

f() ->
  ?A.
