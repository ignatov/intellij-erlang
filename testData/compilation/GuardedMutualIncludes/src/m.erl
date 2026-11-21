-module(m).
-export([f/0]).
-include("a.hrl").
f() -> {?A, ?B}.
