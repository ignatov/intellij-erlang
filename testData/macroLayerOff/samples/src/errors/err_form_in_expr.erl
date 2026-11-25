%% Ошибка: макрос делает целую функцию, а стоит внутри выражения.
%% На верхнем уровне `?DEF(hello).` он верен (см. one_file/m_forms.erl).
%% erlc отвергает файл:
%% erlc: err_form_in_expr.erl:13:8: syntax error before: '->'
%% Здесь должно быть красное на `?DEF(hello)` в функции `f/0`.
-module(err_form_in_expr).

-compile([export_all, nowarn_export_all]).

-define(DEF(Name), Name() -> ok).

f() ->
  ?DEF(hello).
