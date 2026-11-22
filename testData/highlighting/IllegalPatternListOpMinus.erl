-export([f/1, g/1, h/1]).
-define(PREFIX, "abc").

f(<error>?PREFIX -- Rest</error>) -> Rest;
f(<error>?PREFIX -- "c"</error>) -> ok;
f(<error>"abc" -- Rest</error>) -> Rest;
f(<error>[$a] -- Rest</error>) -> Rest.

g(A) -> <error>?PREFIX -- R</error> = A, R.

h(A) -> ?PREFIX -- A.
