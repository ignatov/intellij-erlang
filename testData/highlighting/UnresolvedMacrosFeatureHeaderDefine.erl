-module('UnresolvedMacrosFeatureHeaderDefine').
-export([f/0]).
-include("UnresolvedMacrosFeatureHeaderDefine.hrl").
f() -> {?FEATURE_AVAILABLE(maybe_expr), ?FEATURE_AVAILABLE(a, b), <error descr="Argument mismatch for macro '?FEATURE_AVAILABLE': no definition takes 3 arguments">?FEATURE_AVAILABLE</error>(a, b, c)}.
