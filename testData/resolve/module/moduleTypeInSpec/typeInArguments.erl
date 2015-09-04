-module('typeInArguments').

-spec foo(atom(), module()) -> atom().
foo(A, M) -> ok.

test() -> foo(aaa, 'typeIn<caret>Arguments').