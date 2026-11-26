%% Ошибка: макрос с полем карты `a => 1` стоит вне карты.
%% Внутри карты `#{?FIELD}` он верен (функция `good/0`).
%% erlc отвергает файл:
%% erlc: err_map_field.erl:17:4: syntax error before: '=>'
%% Здесь должно быть красное на `?FIELD` в функции `bad/0`.
%% В функции `good/0` красного быть не должно.
-module(err_map_field).

-compile([export_all, nowarn_export_all]).

-define(FIELD, a => 1).

good() ->
  #{?FIELD}.

bad() ->
  ?FIELD.
