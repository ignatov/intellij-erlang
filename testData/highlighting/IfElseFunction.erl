-export([run/0]).

-if(?OTP_RELEASE >= 21).
f() -> 1.
-elif(true).
f() -> 2.
-else.
f() -> 3.
-endif.

run() -> f().
