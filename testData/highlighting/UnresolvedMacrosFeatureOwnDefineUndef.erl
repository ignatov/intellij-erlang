-module('UnresolvedMacrosFeatureOwnDefineUndef').
-export([f/0, g/0, true/0, true/2]).
-undef(FEATURE_ENABLED).
-define(FEATURE_ENABLED, true).
f() -> ?FEATURE_ENABLED().
g() -> ?FEATURE_ENABLED(a, b).
true() -> ok.
true(_, _) -> ok.
