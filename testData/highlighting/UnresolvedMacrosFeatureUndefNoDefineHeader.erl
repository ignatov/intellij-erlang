-module('UnresolvedMacrosFeatureUndefNoDefineHeader').
-export([f/0]).
-include("UnresolvedMacrosFeatureUndefNoDefineHeader.hrl").
f() -> <error descr="Unresolved macros '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(maybe_expr).
