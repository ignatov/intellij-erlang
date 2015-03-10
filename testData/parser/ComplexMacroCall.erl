-module('ComplexMacroCall').

-define(QMARK, ?).
-define(FOO, foo).

foo(11) ->
  ?QMARK FOO(11).