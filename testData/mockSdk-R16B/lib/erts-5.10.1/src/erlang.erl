-module(erlang).

-export([binary_to_integer/1, binary_to_integer/2]).

%% binary_to_integer/1
-spec binary_to_integer(Binary) -> integer() when
      Binary :: binary().
binary_to_integer(_Binary) ->
    erlang:nif_error(undefined).

is_record(_Term,_RecordTag) ->
    erlang:nif_error(undefined).

is_record(_Term,_RecordTag,_Size) ->
    erlang:nif_error(undefined).