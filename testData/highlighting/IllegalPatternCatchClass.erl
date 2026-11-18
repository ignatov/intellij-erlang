-export([f/1, g/1]).
-define(PREFIX, "abc").

f(A) ->
  try A
  catch
    throw:?PREFIX ++ Rest -> Rest;
    throw:"abc" ++ Rest -> Rest;
    _:[$a] ++ Rest -> Rest;
    error:"abc" ++ Rest:St -> {Rest, St};
    exit:?PREFIX ++ Rest:St -> {Rest, St};
    Class:"a" ++ "b" ++ Rest -> {Class, Rest};
    throw:{"abc" ++ Rest} -> Rest;
    ?PREFIX ++ Rest -> Rest
  end.

g(A) ->
  try A
  catch
    <error>throw:Rest ++ "a"</error> -> Rest;
    <error>throw:"a" -- Rest</error> -> Rest;
    throw:"a" ++ <error>"x" + 1</error> -> ok;
    <error>throw:?MODULE ++ Rest</error> -> Rest
  end.
