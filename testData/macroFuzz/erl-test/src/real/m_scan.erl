%% Код как в реальном проекте: маленький сканер в стиле xmerl_scan.
%% Состояние сканера в записи scan. Макросы меняют состояние и сами
%% связывают переменные: `?bump_col(N)` берёт S0 из головы клауза и
%% связывает S. В охранах стоят обрывки: `?is_digit(C)` раскрывается
%% в две охраны через запятую, `?is_space(C)` в две охраны через `;`.
%%
%% Что попробовать:
%% 1. Поставь курсор на `S` в `scan(T, S)` после `?bump_col(1)` и нажми
%%    Cmd+B. Переменная S связана внутри макроса bump_col.
%% 2. Поставь курсор на `is_space` в охране и нажми Cmd+B.
%% 3. Поставь курсор на `T1` в `word_end/2` после `?strip` и нажми Cmd+B.
%% 4. Поставь курсор на `bump_col` в его определении и нажми Alt+F7.
%% 5. Поставь курсор на поле `col` в `#scan{}` и нажми Alt+F7: должны
%%    найтись вхождения внутри тел макросов.
-module(m_scan).

-export([string/1]).

-record(scan, {line = 1 :: pos_integer(),
               col = 1 :: pos_integer(),
               tokens = [] :: [token()]}).

-type token() :: {atom(), {pos_integer(), pos_integer()}, term()}.

%% Состояние: сдвиг колонки и строки. Связывают S по S0.
-define(bump_col(N), S = S0#scan{col = S0#scan.col + N}).
-define(bump_line, S = S0#scan{line = S0#scan.line + 1, col = 1}).

%% Позиция и новый токен.
-define(pos(St), {St#scan.line, St#scan.col}).
-define(emit(Kind, Value, St),
        St#scan{tokens = [{Kind, ?pos(St), Value} | St#scan.tokens]}).

%% Обрывки для охран.
-define(is_digit(C), C >= $0, C =< $9).
-define(is_space(C), C == $\s; C == $\t).
-define(is_letter(C), C >= $a, C =< $z; C >= $A, C =< $Z).

%% Пропуск пробелов: связывает T1 и S1 по T0 и S0.
-define(strip, {T1, S1} = skip_spaces(T0, S0)).

%% Ошибка с позицией.
-define(fatal(Reason, St),
        erlang:error({scan_error, Reason, ?pos(St)})).

-spec string(string()) -> [token()].
string(Text) ->
  scan(Text, #scan{}).

scan([], S0) ->
  lists:reverse(S0#scan.tokens);
scan([$\n | T], S0) ->
  ?bump_line,
  scan(T, S);
scan([C | T], S0) when ?is_space(C) ->
  ?bump_col(1),
  scan(T, S);
scan([C | _] = T0, S0) when ?is_digit(C) ->
  {Value, Rest, Len} = number(T0, 0, 0),
  ?bump_col(Len),
  scan(Rest, ?emit(int, Value, S0)#scan{col = S#scan.col});
scan([C | _] = T0, S0) when ?is_letter(C) ->
  word(T0, S0);
scan([C | T], S0) when C =:= $(; C =:= $); C =:= $, ->
  ?bump_col(1),
  scan(T, ?emit(punct, C, S0)#scan{col = S#scan.col});
scan([C | _], S0) ->
  ?fatal({bad_char, C}, S0).

word(T0, S0) ->
  {Name, Rest} = lists:splitwith(fun(C) -> not is_stop(C) end, T0),
  ?bump_col(length(Name)),
  word_end(Rest, ?emit(word, list_to_atom(Name), S0)#scan{col = S#scan.col}).

word_end(T0, S0) ->
  ?strip,
  scan(T1, S1).

skip_spaces([C | T], S0) when ?is_space(C) ->
  ?bump_col(1),
  skip_spaces(T, S);
skip_spaces(T, S) ->
  {T, S}.

number([C | T], Acc, Len) when ?is_digit(C) ->
  number(T, Acc * 10 + C - $0, Len + 1);
number(T, Acc, Len) ->
  {Acc, T, Len}.

is_stop(C) when ?is_letter(C) -> false;
is_stop(C) when ?is_digit(C) -> false;
is_stop(_) -> true.
