-module(expr).
-export([x/0]).
-define(X, 1 + 2).
x() -> ?X * 3.
