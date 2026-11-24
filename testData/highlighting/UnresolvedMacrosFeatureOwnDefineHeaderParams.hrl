-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED': no definition takes 0 arguments">?FEATURE_ENABLED</error>().
g() -> ?FEATURE_ENABLED(x).
