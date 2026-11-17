-module(exportOutsideIf_target).
-if(?OTP_RELEASE >= 23).
-export([t1/0]).
-elif(?OTP_RELEASE >= 21).
-export([t2/0]).
-else.
-export([t3/0]).
-endif.
-export([bar/0, f/1]).

-if(?OTP_RELEASE >= 23).
t1() -> ok.
-elif(?OTP_RELEASE >= 21).
t2() -> ok.
-else.
t3() -> ok.
-endif.

bar() -> ok.

f(N) ->
  erlang:error(not_implemented).
