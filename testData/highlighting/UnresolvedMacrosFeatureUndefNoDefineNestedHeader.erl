-module('UnresolvedMacrosFeatureUndefNoDefineNestedHeader').
-export([f/0]).
-include("UnresolvedMacrosFeatureUndefNoDefineNestedHeader.hrl").
f() -> <error descr="Unresolved macros '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(maybe_expr).
