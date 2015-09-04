-module('variableInArguments').

-spec foo(A) -> ok when
  A :: module().

foo(A) -> A.

test() -> foo('variableIn<caret>Arguments').