-module('UnresolvedMacrosFeatureUndefNoDefineOff').
-export([f/0]).
-undef(FEATURE_ENABLED).
f() -> ?FEATURE_ENABLED(maybe_expr).
