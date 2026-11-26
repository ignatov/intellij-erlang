-module('UnresolvedMacrosFeatureQuotedArity').
-export([f/0, g/0, h/0, j/0]).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?'FEATURE_ENABLED'</error>().
g() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?'FEATURE_ENABLED'</error>(a, b).
h() -> <error descr="Argument mismatch for macro '?FEATURE_AVAILABLE'">?'FEATURE_AVAILABLE'</error>.
j() -> ?'FEATURE_ENABLED'(maybe_expr).
