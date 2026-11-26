-module('UnresolvedMacrosFeatureOwnDefineBare').
-export([f/0]).
-define(FEATURE_ENABLED, true).
f() -> is_boolean <error descr="Argument mismatch for macro '?FEATURE_ENABLED'">?FEATURE_ENABLED</error><error descr="'(' unexpected">(</error>a, b).
