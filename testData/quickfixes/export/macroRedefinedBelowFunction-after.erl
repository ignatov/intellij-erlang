-module(m).
-define(test, name).
-export([?test/1]).
?te<caret>st(A) -> A.
-undef(test).
-define(test, other).
