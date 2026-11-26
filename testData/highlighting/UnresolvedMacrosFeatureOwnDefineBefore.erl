-module('UnresolvedMacrosFeatureOwnDefineBefore').
-export([f/0]).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>.
-define(FEATURE_ENABLED, true).
