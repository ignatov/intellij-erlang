-module(macroUndefinitionFromTransitiveInclusion).

-define(X, 10).

-include("headers/transitiveUndefX.hrl").

foo() -> ?X.