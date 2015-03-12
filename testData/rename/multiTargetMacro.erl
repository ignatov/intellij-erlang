-module(multiTargetMacro).

-export([foo/0]).

-ifdef(FOO_1).
-define(FOO, 1).
-else.
-define(FOO, 2).
-endif.

foo() ->
  ?<caret>FOO.