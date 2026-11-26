-module(unused_type_in_uppercase_header_user).
-include("UnusedTypeInUppercaseHeader.HRL").
-export([f/1]).
-spec f(id()) -> ok.
f(_) -> ok.
