-module('UnresolvedMacrosFeatureBare').
-export([f/0]).
f() -> is_boolean ?FEATURE_ENABLED<error descr="'(' unexpected">(</error>maybe_expr).
