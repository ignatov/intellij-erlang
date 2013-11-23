-module(illFormedMacroArgument).

-define(MACRO1(X), X).

foo() -> ?MACRO1(
% this macro call has only whitespace and comment tokens in arguments list, so it is ill-formed as zero-argument macro named MACRO1 is undefined
).

-define(MACRO0(), ok).

bar() -> ?MACRO0(
% in turn, this call is OK as no arguments are passed to a nullary macro named ?MACRO0
).

-define(MACRO2(X, Y), X).

foobar() -> ?MACRO2(
% this macro call is ill-formed
, 10).