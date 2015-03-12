-module(multiTargetMacro).

-export([foo/0]).

-ifdef(FOO_1).
-define(BAR, 1).
-else.
-define(BAR, 2).
-endif.

foo() ->
  ?<caret>BAR.