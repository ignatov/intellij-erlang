-module(varMatchedInComprehensionFilter).
-export([f/1]).

f(L) -> [A<caret> || {A, B} <- L, A = B].
