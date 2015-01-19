-export([foo/0, fun1/1, 'CAPS'/1, ping/1]).

foo() -> ok;
<error>zoo</error>() -> ok;
<error>bar</error>() -> ok.

-define(foo, fun1).
-define(bar, fun1).

?foo(1) -> ok;
?bar(2) -> ok.

'CAPS'(1) -> ok;
'CAPS'(2) -> ok.

-define(with_mismatched_head, ping(1) -> ok; pong(_) -> ok).
<error>?with_mismatched_head</error>.