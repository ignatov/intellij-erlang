-module('IoFormatMapOrderModifiersBefore26').
-export([f/1]).
f(M) ->
  io:format("<warning>~kp~n</warning>", [M]),
  io:format("<warning>~Kp~n</warning>", [reversed, M]),
  io:format("~tp~n", [M]).
