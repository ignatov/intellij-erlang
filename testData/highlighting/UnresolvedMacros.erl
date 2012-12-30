-define(resolved, 1).

-export([foo/0]).

foo() -> ?resolved + 1 + <error>?unresolved</error> + ?LINE.