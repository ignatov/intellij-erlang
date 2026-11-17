-module(m).
-export([other/0]).
-define(test, name).
other() -> ok.
?te<caret>st(A) -> A.
