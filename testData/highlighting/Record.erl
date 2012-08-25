-export([f/1]).
-record(test, {id, name, date}).
f(AAAA) -> AAAA#test{id=1}.