-module(smoke_canary).
-export([run/1, tq/0, cond_val/0, head_bound/1]).

-if(?OTP_RELEASE >= 26).
-define(BIG, true).
-else.
-define(BIG, false).
-endif.
-define(GREETING, "hello").

run(X) ->
    S = ?GREETING " world",
    case X of
        _ -> {S, ?BIG}
    end.

tq() ->
    """
    triple quoted
    """.

cond_val() -> ?BIG.

head_bound(L) ->
    case Y = length(L) of
        0 -> Y;
        N -> N + Y
    end.

unused_one() -> ok.
