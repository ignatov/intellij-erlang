%% Ошибка: обрывок `?PAR` стоит сам по себе, как отдельное выражение.
%% PAR раскрывается в `()`, а `()` без имени функции не выражение.
%% Внутри `foo?PAR` тот же макрос верен (см. one_file/m_fragments.erl).
%% erlc отвергает файл:
%% erlc: err_par_alone.erl:15:4: syntax error before: ')'
%% Здесь должно быть красное на `?PAR,` в функции `f/0`.
-module(err_par_alone).

-compile([export_all, nowarn_export_all]).

-define(PAR, ()).

f() ->
  foo?PAR,
  ?PAR,
  ok.

foo() ->
  ok.
