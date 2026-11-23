%% Условная компиляция по выражению: -if, -elif, -else.
%% Условие использует встроенный макрос OTP_RELEASE.
%% С OTP 29.1 erlc берёт первую ветку.
%% Ещё тут -undef: после него макрос FEATURE снова не определён.
%%
%% Что попробовать:
%% 1. Поставь курсор на `FEATURE` в `feature/0` и нажми Cmd+B. Должно
%%    открыться определение в ветке -if(?OTP_RELEASE >= 27).
%% 2. Поставь курсор на `OTP_RELEASE` в -if и нажми Cmd+B.
%% 3. Посмотри на `-ifdef(FEATURE)` после -undef: ветка внутри не
%%    компилируется, функции `never/0` нет.
-module(m_if_release).

-export([feature/0, defined_after_undef/0]).

-if(?OTP_RELEASE >= 27).
-define(FEATURE, new).
-elif(?OTP_RELEASE >= 25).
-define(FEATURE, middle).
-else.
-define(FEATURE, old).
-endif.

feature() ->
  ?FEATURE.

-undef(FEATURE).

-ifdef(FEATURE).
-export([never/0]).
never() -> ?FEATURE.
-endif.

defined_after_undef() ->
  false.
