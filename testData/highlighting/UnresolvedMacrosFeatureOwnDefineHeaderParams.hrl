-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>().
g() -> ?FEATURE_ENABLED(x).
