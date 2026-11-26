-module(unused_type_in_header_user).
-include("UnusedTypeInHeader.hrl").
-export([f/1]).
-spec f(id()) -> ok.
f(_) -> ok.
