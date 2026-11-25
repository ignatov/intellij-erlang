-module('UnresolvedMacrosFeatureUndefNoDefine').
-export([f/0]).
-undef(FEATURE_ENABLED).
f() -> <error descr="Unresolved macros '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(maybe_expr).
