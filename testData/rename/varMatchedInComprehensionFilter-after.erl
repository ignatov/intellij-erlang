-module(varMatchedInComprehensionFilter).
-export([f/1]).

f(L) -> [Zz<caret> || {Zz, B} <- L, Zz = B].
