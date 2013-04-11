-export([a/1]).

a(A) ->
  b(c(case A of
    1 -> ok;
    { T, R }  -> { T, R }
  end
  )),
  ok.

b(X) -> X.
c(Y) -> Y.