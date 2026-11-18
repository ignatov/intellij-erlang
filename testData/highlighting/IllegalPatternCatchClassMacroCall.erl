-export([f/1, g/1]).
-define(STR(X), "abc").

f(Y) ->
  try Y
  catch
    throw:?STR(1) ++ R -> R;
    exit:?STR(1) ++ R = Z -> {R, Z};
    error:?STR(1) ++ R:St -> {R, St};
    _:?STR(1) ++ R when R =/= [] -> R
  end.

g(Y) ->
  try Y
  catch
    <error>throw:?STR(1) -- R</error> -> ok
  end.
