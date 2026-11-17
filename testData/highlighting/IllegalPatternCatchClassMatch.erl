-export([f/1, g/1]).
-define(PREFIX, "abc").

f(A) ->
  try A
  catch
    throw:"a" ++ R = X -> {X, R};
    throw:?PREFIX ++ R = X -> {X, R};
    throw:[$a] ++ R = X:St -> {X, R, St};
    throw:"a" ++ R = "a" ++ Q -> {R, Q};
    throw:X = "a" ++ R -> {X, R};
    throw:"a" ++ R = X = Y -> {X, Y, R};
    throw:{"a" ++ R} = X -> {X, R}
  end.

g(A) ->
  try A
  catch
    <error>throw:R ++ "a"</error> = X -> X;
    <error>throw:"a" -- R</error> = X -> X;
    <error>throw:?MODULE ++ R</error> = X -> X
  end.
