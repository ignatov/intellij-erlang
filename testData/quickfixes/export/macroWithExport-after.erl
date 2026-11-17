-module(m).
-define(test, name).
-export([other/0, ?test/1]).

other() -> ok.
?te<caret>st(A) -> A.
