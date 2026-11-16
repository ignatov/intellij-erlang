-export([get_count/1]).

-define(THINGS_IDX, <<"TI.">>).
-define(THINGS_LINKED_INDEXNAME, <<?THINGS_IDX/binary, APIKey/binary>>).

get_count(APIKey) ->
  byte_size(?THINGS_LINKED_INDEXNAME).
