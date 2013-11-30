-define(FOO, foo() -> ok).

?FOO.

-define(BAR, bar() ->).

?BAR<fold text='bar/0 -> ...'> ok.</fold>