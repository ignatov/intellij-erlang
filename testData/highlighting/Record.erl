-export([f/1]).
-record(test, {id, name, date}).
f(AAAA) -> AAAA#test{id=1}, AAAA#test{name=#test{name = 1, <error>aaa</error>=1}}.