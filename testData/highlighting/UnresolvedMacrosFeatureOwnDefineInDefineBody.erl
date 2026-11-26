-module('UnresolvedMacrosFeatureOwnDefineInDefineBody').
-export([f/0]).
-define(FEATURE_ENABLED, true).
-define(A, ?FEATURE_ENABLED()).
f() -> ok.
