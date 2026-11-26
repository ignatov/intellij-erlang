%% Макросы в разных местах кода: выражение, образец, охрана (guard),
%% тип и spec, значение поля записи по умолчанию, имя записи, ключ карты,
%% размер сегмента бинарника, атрибут модуля.
%%
%% Что попробовать:
%% 1. Поставь курсор на `IS_POS` в охране функции `check/1` и нажми Cmd+B.
%% 2. Поставь курсор на `REC` в `#?REC{}` (функция `make/0`) и нажми Cmd+B.
%%    Потом поставь курсор на поле `size` там же и нажми Cmd+B: должно
%%    открыться поле записи `item`.
%% 3. Поставь курсор на `ID_TYPE` в `-type` и нажми Alt+F7. Должны найтись
%%    вхождения в `-type`, в `-spec` и в поле записи.
%% 4. Поставь курсор на `OK` в образце функции `is_ok/1` и нажми Shift+F6.
%% 5. Набери `#?` в теле `make/0` и нажми Ctrl+Space.
-module(m_positions).

-export([check/1, make/0, is_ok/1, key/1, header/1, version/0]).

-define(VSN, "1.2.3").
-define(OK, {ok, _}).
-define(IS_POS(V), is_integer(V) andalso V > 0).
-define(ID_TYPE, non_neg_integer()).
-define(DEFAULT_SIZE, 10).
-define(REC, item).
-define(KEY, id).
-define(BITS, 16).

%% Атрибут модуля.
-vsn(?VSN).

%% Тип и spec.
-type id() :: ?ID_TYPE.

%% Имя записи и значение поля по умолчанию.
-record(?REC, {id :: ?ID_TYPE | undefined,
               size = ?DEFAULT_SIZE :: integer()}).

%% Охрана.
-spec check(term()) -> boolean().
check(V) when ?IS_POS(V) -> true;
check(_) -> false.

%% Имя записи в выражении и в образце.
-spec make() -> #item{}.
make() ->
  R = #?REC{id = 1},
  #?REC{size = S} = R,
  R#?REC{size = S + 1}.

%% Образец.
is_ok(?OK) -> true;
is_ok(_) -> false.

%% Ключ карты в выражении и в образце.
-spec key(id()) -> map().
key(Id) ->
  M = #{?KEY => Id},
  #{?KEY := Same} = M,
  M#{?KEY => Same + 1}.

%% Размер сегмента бинарника.
header(<<Len:?BITS, Rest/binary>>) ->
  {Len, Rest, <<Len:?BITS>>}.

version() ->
  ?VSN.
