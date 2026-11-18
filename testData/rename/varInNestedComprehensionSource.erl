-module(varInNestedComprehensionSource).
-export([f/1]).

f(L) -> [X || X <- [X || X<caret> <- L, X > 0]].
