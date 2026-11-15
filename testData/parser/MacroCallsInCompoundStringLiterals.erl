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
