-module('UnresolvedMacrosFeatureArity').
-export([f/0, g/0, h/0, i/0, j/0]).
-define(P, ?FEATURE_ENABLED).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>.
g() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>().
h() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(a, b).
i() -> <error descr="Argument mismatch for macro '?FEATURE_AVAILABLE'">?FEATURE_AVAILABLE</error>.
j() -> ?FEATURE_ENABLED(maybe_expr).
