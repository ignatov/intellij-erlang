-module('UnresolvedMacrosFeatureUndefOwnDefine').
-export([f/0]).
-undef(FEATURE_ENABLED).
-define(FEATURE_ENABLED(A, B), {A, B}).
f() -> {<error descr="Argument mismatch for macro '?FEATURE_ENABLED': no definition takes 1 argument">?FEATURE_ENABLED</error>(maybe_expr), ?FEATURE_ENABLED(a, b)}.
