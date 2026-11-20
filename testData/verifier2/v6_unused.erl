-module(v6_unused).
-export([api/1, api2/1, api3/1, api4/1]).
-define(CALL_HELPER(X), only_via_macro(X);).
-define(USED_ONLY_IN_BODY, 42).
-define(FRAG(V), V + ?USED_ONLY_IN_BODY;).
-define(ARG(E), E;).

api(1) -> ?CALL_HELPER(1)
api(2) -> ?ARG(only_in_arg(2))
api(N) when is_integer(N) -> ?FRAG(N * 3)
api(_) -> 0.

api2(N) when N > 0 -> Vv = N + 1, ?ARG(Vv)
api2(_) -> 0.

api3(N) when N > 0 -> ?ARG(missing_fn(N))
api3(_) -> 0.

api4(N) when N > 0 -> ?ARG(begin Unused = N, 2 end)
api4(_) -> 0.

only_via_macro(X) -> X * 2.

only_in_arg(X) -> X * 4.
