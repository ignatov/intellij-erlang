-module(pat).
-export([f/1]).
-define(PAT, {ok, Value}).
f(?PAT) -> Value;
f(_) -> none.
