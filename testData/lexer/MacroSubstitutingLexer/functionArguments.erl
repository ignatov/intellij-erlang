-module(functionArguments).

-define(MACRO, bar).

foo() ->
  ?MACRO().

bar() -> ok.