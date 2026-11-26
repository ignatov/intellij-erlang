-module('UnresolvedMacrosOtpRelease').
-export([release/0, at_least/1]).

-if(?OTP_RELEASE >= 27).
-define(NEW, true).
-else.
-define(NEW, false).
-endif.

release() -> {?OTP_RELEASE, ?NEW}.

at_least(N) when ?OTP_RELEASE >= N -> true;
at_least(_) -> false.
