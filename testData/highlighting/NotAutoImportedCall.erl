-export([bar/0]).

bar() ->
  <warning descr="Unresolved function md5/1">md5</warning>("1"),
  <warning descr="Unresolved function crc32/1">crc32</warning>("1"),
  erlang:md5("1"),
  erlang:crc32("1"),
  abs(-1),
  spawn(fun bar/0),
  ok.
