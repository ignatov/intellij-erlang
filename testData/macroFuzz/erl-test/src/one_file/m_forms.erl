%% Макросы, которые делают целые формы: функцию, запись, -export,
%% несколько клаузов одной функции.
%%
%% Что попробовать:
%% 1. Поставь курсор на `hello` в `hello()` (функция `use/0`) и нажми Cmd+B.
%%    Курсор должен прыгнуть на `?DEF(hello).`
%% 2. Поставь курсор на `config` в `#config{}` и нажми Cmd+B. Запись
%%    объявлена только макросом `?REC(config).`
%% 3. Поставь курсор на `count` в `count(3)` и нажми Cmd+B. Три клауза
%%    функции `count/1` сделаны одним вхождением `?CLAUSES(count).`
%% 4. Проверь, что `exported/0` не помечена как неиспользуемая:
%%    её экспортирует `?EXPORT(exported).`
-module(m_forms).

-export([use/0]).

%% Функция без аргументов.
-define(DEF(Name), Name() -> ok).

%% Запись с полями a и b.
-define(REC(Name), -record(Name, {a = 1, b = 2})).

%% Атрибут -export.
-define(EXPORT(Name), -export([Name/0])).

%% Несколько клаузов одной функции.
-define(CLAUSES(Name),
        Name(0) -> zero;
        Name(1) -> one;
        Name(_) -> many).

%% Функция с телом из аргумента.
-define(GETTER(Name, Value), Name() -> Value).

%% -export должен стоять до первой функции.
?EXPORT(exported).
?REC(config).
?DEF(hello).
?CLAUSES(count).
?GETTER(answer, 42).
?GETTER(greeting, "hi").

exported() ->
  done.

use() ->
  C = #config{a = 10},
  {hello(), C#config.b, count(3), answer(), greeting()}.
