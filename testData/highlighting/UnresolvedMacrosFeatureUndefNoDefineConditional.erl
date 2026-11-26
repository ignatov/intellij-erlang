-module('UnresolvedMacrosFeatureUndefNoDefineConditional').
-export([f/0]).
-ifdef(NEVER).
-undef(FEATURE_ENABLED).
-endif.
f() -> ?FEATURE_ENABLED(maybe_expr).
