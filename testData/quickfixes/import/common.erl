-import(incl, [
%% comment
crc32/1
, crc32/2
]).
-import(incl, [foo/0, bar/0]).
-import(erlang, [asdfadf/1]).
-import(erlang, [dt_get_tag/0]).
-export([crc32/1]).

crc32<caret>(Data) -> Data.