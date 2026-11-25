-module('UnresolvedMacrosFeatureUndefNoDefineCases').
-export([f/0, g/0, h/0, i/0]).
-define(BODY, ?FEATURE_AVAILABLE(maybe_expr)).
f() -> ?FEATURE_AVAILABLE(maybe_expr).
-undef(FEATURE_AVAILABLE).
-undef(FEATURE_ENABLED).
g() -> <error descr="Unresolved macros '?FEATURE_AVAILABLE'">?FEATURE_AVAILABLE</error>(maybe_expr).
h() -> is_boolean <error descr="Unresolved macros '?FEATURE_ENABLED'">?FEATURE_ENABLED</error><error descr="'(' unexpected">(</error>maybe_expr).
i() -> <error descr="Unresolved macros '?FEATURE_ENABLED'">?FEATURE_ENABLED</error>(a, b).
