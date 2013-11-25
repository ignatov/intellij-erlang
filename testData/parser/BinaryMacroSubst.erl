-define(MACRO(X, Y), X + Y).

foo() -> ?MACRO(5, 10).
