-module('IoFormatSigilWithHashDelimiter').
-export([f/0, delimiters/0]).

f() ->
  io:format(~s"<warning descr="Invalid control sequence">~</warning>", [1]),
  io:format(~s#<warning descr="Invalid control sequence">~</warning>#, [1]),
  io:format(~S#<warning descr="Invalid control sequence">~</warning>#, [1]).

delimiters() ->
  io:format(~s(<warning descr="Invalid control sequence">~</warning>), [1]),
  io:format(~s[<warning descr="Invalid control sequence">~</warning>], [1]),
  io:format(~s{<warning descr="Invalid control sequence">~</warning>}, [1]),
  io:format(~s<<warning descr="Invalid control sequence">~</warning>>, [1]),
  io:format(~s/<warning descr="Invalid control sequence">~</warning>/, [1]),
  io:format(~s|<warning descr="Invalid control sequence">~</warning>|, [1]),
  io:format(~s'<warning descr="Invalid control sequence">~</warning>', [1]),
  io:format(~s`<warning descr="Invalid control sequence">~</warning>`, [1]),
  io:format(~s#a <warning descr="Invalid control sequence">~q b</warning>#, [1]),
  io:format(<warning descr="Wrong number of arguments in format call, should be 1">~s#~p#</warning>, []),
  io:format(~s#~p#, [1]).
