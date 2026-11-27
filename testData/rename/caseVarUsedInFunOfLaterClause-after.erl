-module(caseVarUsedInFunOfLaterClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> <caret>New = 1;
        _ -> New = 2, _ = fun() -> New end
    end.
