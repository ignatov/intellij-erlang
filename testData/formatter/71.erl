-module(aaa).

foo() ->
f(fun() -> f()                                     end).