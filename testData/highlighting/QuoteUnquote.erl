-export([foo/0, foo2/0, foo3/0, f/0]).

-record(msg_full_object, { action }).
-record('2', { action }).
-record('a', { action }).

foo() -> #'msg_full_object'{ action = undefined }.
foo2() -> #'2'{ action = undefined }.
foo3() -> #a{ action = undefined }, #a{}, f().

<warning>'f'</warning>() -> ok.
