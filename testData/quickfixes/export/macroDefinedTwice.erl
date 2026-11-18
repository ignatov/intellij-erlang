-module(m).
-define(test, old_name).
-undef(test).
-define(test, name).

?te<caret>st(A) -> A.
