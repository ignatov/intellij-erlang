-export([foo/0, ?foo/1]).

foo() -> ok;
<error>zoo</error>() -> ok;
<error>bar</error>() -> ok.

-define(foo, fun1).
-define(bar, fun1).

?foo(1) -> ok;
?bar(2) -> ok.