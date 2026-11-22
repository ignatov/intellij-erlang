-module(ifAttribute).

-if(?OTP_RELEASE >= 27).
f() -> new.
-elif(?OTP_RELEASE >= 25).
f() -> mid.
-else.
f() -> old.
-endif.
