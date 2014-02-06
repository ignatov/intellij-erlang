-export([foo/0, bar/0]).


foo() ->
  <warning>X</warning> = 14.

-define(MACRO, X + 1).

bar() ->
  X = 14,
  ?MACRO.