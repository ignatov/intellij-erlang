-module('UnresolvedMacrosFeatureOwnDefineOneArgument').
-export([f/0]).
-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> {?FEATURE_ENABLED(maybe_expr), ?FEATURE_ENABLED(a, b)}.
