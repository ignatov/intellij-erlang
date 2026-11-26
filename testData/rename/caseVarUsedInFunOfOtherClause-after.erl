-module(caseVarUsedInFunOfOtherClause).
-export([f/1]).
f(X) ->
    case X of
        1 -> New = 1, _ = fun() -> New end;
        _ -> <caret>New = 2
    end.
