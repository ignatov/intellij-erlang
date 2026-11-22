-module('UnresolvedMacrosFeature').
-export([f/0, g/0]).
f() -> ?FEATURE_ENABLED(maybe_expr).
g() -> ?FEATURE_AVAILABLE(maybe_expr).
