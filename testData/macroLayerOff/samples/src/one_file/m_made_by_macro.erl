%% Имена, которые есть только в теле или в аргументе макроса:
%% функция, запись, поле записи и переменная. Обычный код ниже их
%% использует. Тут проверяем переход, поиск, переименование и
%% автодополнение таких имён.
%%
%% Что попробовать:
%% 1. Поставь курсор на `area` в `area(R)` (функция `run/0`) и нажми Cmd+B.
%%    Функция `area/1` объявлена только вхождением `?FUN1(area, ...)`.
%% 2. Поставь курсор на поле `width` в `R#shape.width` и нажми Cmd+B.
%%    Поле есть только в аргументе `?RECORD(shape, width, height)`.
%% 3. Поставь курсор на `Total` в `Total + 1` и нажми Cmd+B. Переменная
%%    связана внутри `?BIND(Total, ...)`.
%% 4. Нажми Alt+F7 на `area` в `?FUN1(area, ...)`, потом Shift+F6.
%% 5. В `run/0` набери `R#shape.` и нажми Ctrl+Space: должны быть
%%    поля `width` и `height`.
-module(m_made_by_macro).

-export([run/0]).

%% Запись с двумя полями: имя записи и полей приходят аргументами.
-define(RECORD(Name, F1, F2), -record(Name, {F1 = 0, F2 = 0})).

%% Функция с одним аргументом.
-define(FUN1(Name, Arg, Body), Name(Arg) -> Body).

%% Связывание переменной.
-define(BIND(Var, Value), Var = Value).

?RECORD(shape, width, height).
?FUN1(area, S, S#shape.width * S#shape.height).
?FUN1(perimeter, S, 2 * (S#shape.width + S#shape.height)).

run() ->
  R = #shape{width = 3, height = 4},
  ?BIND(Total, area(R) + perimeter(R)),
  {R#shape.width, Total + 1}.
