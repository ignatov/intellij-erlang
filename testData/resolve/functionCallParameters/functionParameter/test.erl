-module(test).
-import(incl, [bar/0]).

foo(Function) -> Function.

test() -> foo(ba<caret>r).
