-module('UnresolvedMacrosFeatureOwnDefine').
-export([f/0, g/0]).
-define(FEATURE_ENABLED, true).
-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> ?FEATURE_ENABLED.
g() -> ?FEATURE_ENABLED(a, b).
