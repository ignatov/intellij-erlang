-module(uses_hrl).
-include("defs_smoke.hrl").
-export([limit/0]).

limit() -> ?SMOKE_LIMIT.
