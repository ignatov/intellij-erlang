-module('UnresolvedMacrosFeatureOwnDefineParens').
-export([f/0]).
-define(FEATURE_ENABLED, true).
f() -> <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>().
