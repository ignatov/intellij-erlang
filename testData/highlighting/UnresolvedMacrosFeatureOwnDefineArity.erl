-module('UnresolvedMacrosFeatureOwnDefineArity').
-export([f/0, g/0, h/0, i/0, j/0]).
-define(FEATURE_ENABLED, true).
-define(FEATURE_AVAILABLE, true).
f() -> ?FEATURE_ENABLED.
g() -> ?FEATURE_ENABLED(maybe_expr).
h() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(a, b).
i() -> <error descr="Argument mismatch for macro '?FEATURE_AVAILABLE'">?FEATURE_AVAILABLE</error>().
j() -> ?FEATURE_AVAILABLE(maybe_expr).
