-module('UnresolvedMacrosFeatureUndefNoDefineHeaderConditional').
-export([f/0]).
-include("UnresolvedMacrosFeatureUndefNoDefineHeaderConditional.hrl").
f() -> ?FEATURE_ENABLED(maybe_expr).
