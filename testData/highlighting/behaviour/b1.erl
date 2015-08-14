-module(b1).
-callback init0() -> ok.
-callback init1(ok) -> ok.
-callback optional() -> ok.

-optional_callbacks([optional/0]).