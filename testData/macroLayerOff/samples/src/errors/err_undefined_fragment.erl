%% Ошибка: неопределённый макрос там, где обычно стоит обрывок:
%% после имени функции, вместо скобок вызова.
%% erlc отвергает файл:
%% erlc: err_undefined_fragment.erl:11:7: undefined macro 'NO_ARGS'
%% Здесь должно быть красное на `?NO_ARGS` в функции `f/0`.
-module(err_undefined_fragment).

-compile([export_all, nowarn_export_all]).

f() ->
  foo?NO_ARGS.

foo() ->
  ok.
