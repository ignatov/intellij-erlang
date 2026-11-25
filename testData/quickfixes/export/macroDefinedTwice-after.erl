-module(m).
-define(test, old_name).
-undef(test).
-define(test, name).
-export([?test/1]).

?te<caret>st(A) -> A.
