-module(commas).
-export([t/0, c/0]).
-define(PAIR, 1, 2).
-define(ARGS(A), A, A).
t() -> {?PAIR, 3}.
c() -> lists:max([?ARGS(4), 5]) + max(?PAIR).
