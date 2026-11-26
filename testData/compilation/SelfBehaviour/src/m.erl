-module(m).
-behaviour(m).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [];
behaviour_info(_) -> undefined.
