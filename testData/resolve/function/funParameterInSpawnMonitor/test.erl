-module(test).

foo(A) -> ok.

test() -> erlang:spawn_monitor(test, fo<caret>o, [ok]).