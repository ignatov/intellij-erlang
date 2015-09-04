-module(test).

-spec foo(module(), atom(), list()) -> atom().
foo(Module, Function, List) -> ok.

bar(A) -> A.

test() -> foo(module, ba<caret>r, [ok]).