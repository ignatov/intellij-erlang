-module('IoFormatSigilWithHashDelimiter').
-export([f/0]).

f() ->
  io:format(~s"<warning descr="Invalid control sequence">~</warning>", [1]),
  io:format(~s#<warning descr="Invalid control sequence">~</warning>#, [1]),
  io:format(~S#<warning descr="Invalid control sequence">~</warning>#, [1]).
