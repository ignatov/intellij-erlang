-module(varInNestedComprehensionSource).
-export([f/1]).

f(L) -> [X || X <- [Zz || Zz<caret> <- L, Zz > 0]].
