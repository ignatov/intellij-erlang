-module(questionMarkInMacro).

-define(QMARK, ?).
-define(MACRO, ok).

foo() -> ?QMARK MACRO.