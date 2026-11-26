%% Основы: всё в одном файле, без заголовков.
%% Тут константы, макросы с аргументами, одно имя с двумя арностями,
%% вложенные макросы, встроенные макросы и строка из аргумента (??X).
%%
%% Что попробовать:
%% 1. Поставь курсор на `TIMEOUT` в функции `timeout/0` и нажми Cmd+B.
%%    Курсор должен прыгнуть на `-define(TIMEOUT, 5000).`
%% 2. Поставь курсор на `M` в `?M(2)` и нажми Cmd+B. Должно открыться
%%    определение `M(X)`, а не `M` без аргументов. Для `?M` в `arity/0`
%%    наоборот.
%% 3. Поставь курсор на `DOUBLE` в его определении и нажми Alt+F7.
%%    Должны найтись вхождения в `nested/1` и внутри `QUAD`.
%% 4. Поставь курсор на `SQUARE` и нажми Shift+F6: переименуй макрос.
%%    Должны поменяться определение и все вхождения.
%% 5. В функции `predefined/0` набери `?` и нажми Ctrl+Space.
%%    В списке должны быть свои макросы и встроенные (`MODULE`, `LINE`).
-module(m_basics).

-export([timeout/0, arity/0, square/1, nested/1, predefined/0, show/1]).

%% Константы.
-define(TIMEOUT, 5000).
-define(NAME, "basics").

%% Макрос с аргументом.
-define(SQUARE(X), ((X) * (X))).

%% Одно имя, две арности: это два разных определения.
-define(M, 1).
-define(M(X), X + 100).

%% Вложенные макросы: тело одного макроса использует другой.
-define(DOUBLE(X), (2 * (X))).
-define(QUAD(X), ?DOUBLE(?DOUBLE(X))).

%% Строка из аргумента: ??Expr даёт текст выражения.
-define(SHOW(Expr), io_lib:format("~s = ~p", [??Expr, Expr])).

timeout() ->
  {?TIMEOUT, ?NAME}.

arity() ->
  {?M, ?M(2)}.

square(N) ->
  ?SQUARE(N + 1).

nested(N) ->
  {?DOUBLE(N), ?QUAD(N)}.

predefined() ->
  #{module => ?MODULE,
    module_string => ?MODULE_STRING,
    file => ?FILE,
    line => ?LINE,
    function => {?FUNCTION_NAME, ?FUNCTION_ARITY},
    otp => ?OTP_RELEASE}.

show(N) ->
  ?SHOW(N * 2 + 1).
