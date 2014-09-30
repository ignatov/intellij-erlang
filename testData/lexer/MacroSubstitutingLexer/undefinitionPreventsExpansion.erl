-module(undefinitionPreventsExpansion).

-define(MACRO, 10).

foo() -> ?MACRO.

-undef(MACRO).

bar() -> ?MACRO.