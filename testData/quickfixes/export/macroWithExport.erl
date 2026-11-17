-module(m).
-define(test, name).
-export([other/0]).

other() -> ok.
?te<caret>st(A) -> A.
