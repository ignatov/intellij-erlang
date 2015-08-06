-module(behaviour_info_module).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{encode_message, 3}];
behaviour_info(_) ->
  undefined.