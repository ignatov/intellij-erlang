-define(D(X), ?LINE, X).

f() ->
  ?D(1),
  ok.
