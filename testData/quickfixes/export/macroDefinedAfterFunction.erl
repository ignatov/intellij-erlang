-module(m).
-export([other/0]).
other() -> ok.
-define(test, name).
?te<caret>st(A) -> A.
