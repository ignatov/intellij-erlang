-module(erlang).

-export([crc32/1, crc32/2]).

%% crc32/1
-spec erlang:crc32(Data) -> non_neg_integer() when
      Data :: iodata().
crc32(_Data) ->
    erlang:nif_error(undefined).
