-module(test).

foo(Module) -> Module.

test() -> foo(inc<caret>l).
