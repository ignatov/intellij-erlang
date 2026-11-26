-module('IoFormatMapOrderModifiers').
-export([f/1]).
f(M) ->
  io:format("~kp~n", [M]),
  io:format("~Kp~n", [reversed, M]),
  io:format("~tkp~n", [M]).
