-module('IoFormatMapOrderModifiersChecks').
-export([f/1]).
f(M) ->
  io:format("~kw ~kP ~10.4kp ~lkp~n", [M, M, 3, M, M]),
  io:format("~*.*.*KP~n", [10, 10, 10, ordered, M, 3]),
  io:format(~S"~Kp\n", [reversed, M]),
  io:format("~KW~n", [ordered, M, 3]),
  io:format("~kW~n", [M, 3]),
  io:format(<warning>"~Kp~n"</warning>, [M]),
  io:format("<warning>~kkp~n</warning>", [M]),
  io:format("<warning>~kKp~n</warning>", [reversed, M]),
  io:format("<warning>~tlp~n</warning>", [M]),
  io:format("~p <warning>~kc~n</warning>", [M, $a]),
  io:format("<warning>~ks~n</warning>", [M]).
