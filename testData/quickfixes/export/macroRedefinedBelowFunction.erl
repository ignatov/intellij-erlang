-module(m).
-define(test, name).
?te<caret>st(A) -> A.
-undef(test).
-define(test, other).
