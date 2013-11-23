-module(macroWithRightParentheses).

-define(MACRO, ok))).

foo() -> ((?MACRO.