-module(parameterTextSubstitution).

-define(STRING(X), ??X).

foo() -> ?STRING(bar() -> ok).