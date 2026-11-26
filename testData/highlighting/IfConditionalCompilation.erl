-module('IfConditionalCompilation').
-export([f/0, g/0]).

-define(LEVEL, 2).

-if(?LEVEL >= 2).
-define(NAME, high).
-elif(?LEVEL >= 1).
-define(NAME, low).
-else.
-define(NAME, none).
-endif.

-if(defined(LEVEL)).
-define(DEFINED, true).
-else.
-define(DEFINED, false).
-endif.

f() -> ?NAME.

g() -> ?DEFINED.
