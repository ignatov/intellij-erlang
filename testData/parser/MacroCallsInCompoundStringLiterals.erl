-module('MacroCallsInCompoundStringLiterals').

pretty_print_module_name() ->
  "Module" ?MODULE_STRING.

-define(M, "m").

macro_then_string() ->
  ?MODULE_STRING":f".

macro_then_string_with_space() ->
  ?MODULE_STRING ":f".

user_macro_then_string() ->
  ?M "x".

macros_then_string() ->
  ?M ?MODULE_STRING "x" ?M.

string_macro_string() ->
  "a" ?M "x".

-define(S(X), ??X " failed").

stringified_arg_then_string() ->
  ?S(a).

-define(L(X), io:format(?MODULE_STRING ": " ??X " = ~p~n", [X])).
-define(T(X), ?M "x" ??X).

stringified_arg_after_macro_then_string() ->
  ?L(1), ?T(a).

-record(r, {a = ?M "x", b = 1}).

macro_then_string_in_head(?M "cd", X) -> {X, #r{}}.
