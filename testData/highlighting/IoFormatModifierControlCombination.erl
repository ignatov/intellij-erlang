-export([bar/1]).
bar(T) ->
  io:format("<warning descr="Invalid control sequence">~tx</warning>", [255, "0x"]),
  io:format("<warning descr="Invalid control sequence">~lW</warning>", [T, 9]),
  io:format("<warning descr="Invalid control sequence">~ln</warning>", []),
  io:format("~tW", [T, 9]),
  io:format("~lp", [T]),
  io:format("~ts", [T]),
  io:format("<warning descr="Invalid control sequence">~tn~t~</warning>", []),
  io:format("<warning descr="Invalid control sequence">~ls</warning>", [T]),
  io:format("~p <warning descr="Invalid control sequence">~lc</warning>", [T, $a]),
  io:format("~tc~tP~tw~lP", [$a, T, 9, T, T, 9]).
