-module(test).

bar() -> ok.
bar(A) -> A.
bar(A, B) -> ok.

-spec foo(module(), atom(), list()) -> atom().
foo(Module, Function, List) -> ok.

test() -> foo(test, ba<caret>r, [ok]).