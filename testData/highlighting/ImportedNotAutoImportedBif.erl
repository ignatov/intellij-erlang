-include("ImportedNotAutoImportedBif.hrl").
-import(erlang, [md5/1]).
-export([bar/0]).

bar() ->
  md5("1"),
  crc32("1"),
  <warning descr="Unresolved function adler32/1">adler32</warning>("1").
