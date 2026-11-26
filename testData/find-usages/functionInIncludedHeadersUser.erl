-module(functionInIncludedHeadersUser).
-export([f/0]).
-include("functionInFirstIncludedHeader.hrl").
-include("functionInLaterIncludedHeader.hrl").
f() -> g().
