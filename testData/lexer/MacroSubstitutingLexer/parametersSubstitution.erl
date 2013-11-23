-module(parametersSubstitution).

-define(ADD(X, Y), X + Y).

foo() -> ?ADD(10, 123).

?ADD(bar() -> 10, 123).