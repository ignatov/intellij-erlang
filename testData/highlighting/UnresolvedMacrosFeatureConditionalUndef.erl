-module('UnresolvedMacrosFeatureConditionalUndef').
-export([f/0]).
-ifdef(NEVER).
-undef(FEATURE_ENABLED).
-endif.
-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> ?FEATURE_ENABLED(maybe_expr).
