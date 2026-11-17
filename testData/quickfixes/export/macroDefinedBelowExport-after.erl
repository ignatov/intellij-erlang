-module(m).
-export([other/0]).
-define(test, name).
-export([?test/1]).
other() -> ok.
?te<caret>st(A) -> A.
