-module(m).
-export([other/0]).
-define(test, name).
-export([another/0, ?test/1]).

other() -> ok.
another() -> ok.
?te<caret>st(A) -> A.
