-module('UnresolvedMacrosFeatureUndefNoDefineHeaderConditionalInclude').
-export([f/0]).
-include("UnresolvedMacrosFeatureUndefNoDefineHeaderConditionalInclude.hrl").
f() -> ?FEATURE_ENABLED(maybe_expr).
