%% Обрывки: макросы, тело которых не целое выражение.
%% Это главное, что проверяем. erlc принимает весь файл.
%%
%% Что попробовать:
%% 1. Поставь курсор на `PAR` в `foo?PAR` (функция `par/0`) и нажми Cmd+B.
%%    Потом нажми Cmd+B на `foo` там же: должна открыться функция `foo/0`.
%% 2. Поставь курсор на `add` в `add?ARGS(V)` (функция `args/1`) и нажми
%%    Cmd+B. Должна открыться `add/2`: аргументы пришли из обрывка.
%% 3. Поставь курсор на `TAIL`, `BEGIN`, `END` и нажми Alt+F7.
%% 4. Поставь курсор на `reverse` в `?LISTS reverse(L)` и нажми Cmd+B.
%%    Должна открыться `lists:reverse/1`.
%% 5. Проверь, что в файле нет красного: erlc его компилирует.
-module(m_fragments).

-export([par/0, args/1, tail/1, block/0, sign/1, small/1, plus/1,
         nested/0, prefix/1, kind/1, list/0, arrow/0]).

-define(PAR, ()).
-define(ARGS(X), (X, 1)).
-define(TAIL(X), , X]).
-define(BEGIN, begin).
-define(END, end).
-define(HEAD(N), sign(N) when N > 0 ->).
-define(AND_SMALL(N), , N < 100).
-define(PLUS_ONE, 1 +).
-define(ID(X), X).
-define(LISTS, lists:).
-define(CASE(E), case E of).
-define(OPEN, [).
-define(CLOSE, ]).
-define(ARROW, ->).

%% Пустые скобки вызова.
par() ->
  foo?PAR,
  foo?PAR.

foo() ->
  ok.

%% Список аргументов.
args(V) ->
  add?ARGS(V).

add(A, B) ->
  A + B.

%% Хвост списка.
tail(X) ->
  [1 ?TAIL(X).

%% Начало и конец блока.
block() ->
  ?BEGIN
    A = 1,
    A + 1
  ?END.

%% Голова клауза.
?HEAD(N) positive;
sign(0) -> zero;
sign(_) -> negative.

%% Часть охраны.
small(N) when is_integer(N) ?AND_SMALL(N) -> true;
small(_) -> false.

%% Макрос кончается оператором.
plus(N) ->
  ?PLUS_ONE N.

%% Обрывок внутри аргументов другого макроса.
nested() ->
  ?ID(foo?PAR).

%% Префикс модуля.
prefix(L) ->
  ?LISTS reverse(L).

%% Начало case.
kind(X) ->
  ?CASE(X)
    0 -> zero;
    _ -> other
  end.

%% Скобки списка.
list() ->
  ?OPEN 1, 2, 3 ?CLOSE.

%% Стрелка клауза.
arrow() ?ARROW
  ok.
