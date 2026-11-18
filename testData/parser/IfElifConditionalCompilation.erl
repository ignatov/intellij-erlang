-module('IfElifConditionalCompilation').
-export([f/0]).

-if(?OTP_RELEASE < 26).
-type dynamic() :: term().
-endif.

-if(?OTP_RELEASE >= 27).
f() -> new.
-elif(?OTP_RELEASE >= 25).
f() -> mid.
-else.
f() -> old.
-endif.
